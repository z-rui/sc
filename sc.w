
\def\POSIX/{{\mc POSIX\spacefactor1000}}

@* Introduction.
This program is inspired by the Unix {\tt dc} (desk calculator) program
and aims to be a {\bf s}cientific {\bf c}alulator.
I would like to mimic the syntax of {\tt dc}, but there will be significant
differences.
\item{$\bullet$}
This program will use floating point arithmetic, as opposed to the fixed-point
(scaled integer) arithmetic used by {\tt dc}.
\item{$\bullet$}
This program will use binary representation instead of decimal.
As a result, the handling of precision will be incompatible with {\tt dc}.

@ This program requires two external dependencies:
\item{$\bullet$}
GNU Multiple Precision Arithmetic Library
(GMP, {\tt http://gmplib.org/}).  As its name suggests, it does
multi-precision arithmetic.  The library implements efficient algorithms,
some of which are written in assembly.  It is widely available for
many platforms and is licensed under Lessor GNU Public License (LGPL).
\item{$\bullet$}
GNU MPFR Library ({\tt http://mpfr.org/}).
This library implements floating point arithmetic that is mostly compliant
to IEEE--754.  It is based on GMP, so it can handle floating point numbers
at any precision.

Other than these libraries, this program only requires a standard C library.
The header files are included here.  To compile, you need the header files
installed on your machine, and link against the GMP and MPFR libraries
(and the perhaps math library).

%define some types from GMP and MPFR
@s mpz_t int
@s mpfr_t int
@s mpz_ptr int
@s mpfr_ptr int
@s mpfr_exp_t int
@s mpfr_prec_t int
@s mpfr_rnd_t int

@c
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <math.h>
#include <gmp.h>
#include <mpfr.h>

@* Utility functions.  In this section, some general purpose functions
are defined and will be used by the rest of the program.

@ For error reporting, we only define a function, |complain|, which
prints a message to the standard error preceded by the program name.

The program name is usually passed to the |main| function.
To have it available everywhere, it is stored in a global variable.
Some people have strong objections against global variables, but I don't
see any problems here.

@c
const char *progname; /* set by |main| on startup */
void complain(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s: ", progname);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}

@ In case of memory allocation failure, the allocation function returns
a null pointer.  Access via this pointer will result in a segmentation fault.
To avoid this rude behavior, we check the pointer after every allocation.
@c
void checkptr(void *p)
{
	if (p == NULL) {
		complain("memory allocation failed! "
			"(I can hold no more in my brain, sorry.)\n");
		exit(1);
	}
}

@ Function |memdup| allocates memory and creates a clone of a memory chunk.
@c
void *memdup(void *base, size_t size)
{
	void *p;

	p = malloc(size);
	checkptr(p);
	return memcpy(p, base, size);
}

@ A string buffer allows adding characters into it and dynamically expand
its capacity.  This is useful for reading from input whose length cannot
be determined beforehand.

@c
char *buf_add(char *buf, size_t *len, size_t *cap, int c)
{
	if (*len == *cap) {
		*cap *= 2;
		buf = realloc(buf, *cap);
		checkptr(buf);
	}
	buf[(*len)++] = c;
	return buf;
}

@ For error reporting purpose, we convert a character token into the
string form.  If the character is printable, the string contains
the character itself and its character code (in octal).
Otherwise, only the character code is included.

@c
const char *tok2str(int tok)
{
	static char buf[11];

	tok &= UCHAR_MAX;
	if (isprint(tok))
		sprintf(buf, "'%c' (%#03o)", tok, tok);
	else
		sprintf(buf, "%#03o", tok);
	return buf;
}

@* Global state.  Some parameters are required in this program, including:
\item{$\bullet$}
Input and output bases.
\item{$\bullet$}
Floating point precision and rounding mode.

These parameters will be stored in global variables.
Again, they are readonly in most procedures of this program
and only changed by explicit user commands.
Thus, I don't see any problems in doing this.
Some people suggested wrapping these global variables inside a structure
to make the all the functions ``re-entrant.''
However, re-entrancy is not a problem here, and such structure will become
a so-called ``singleton'', which I don't think is very different from
a global variable.

@c
int _ibase = 10, _obase = 10; /* input/output base */
mpfr_prec_t _precision = 53; /* floating point precision */
mpfr_rnd_t _rnd_mode = MPFR_RNDN; /* rounding mode */

@* Token scanner.  The input is scanned into tokens for further processing.
There are two kinds of tokens:
\item{$\bullet$} A {\it character token\/} is a literal character in the input.
\item{$\bullet$} A {\it value token\/} is a sequence of characters that
represent a (numeric or string) value.

The input comes from two sources:
\item{$\bullet$} A file (possibly the standard input), for which I should use
functions from \.{<stdio.h>} for reading.
\item{$\bullet$} A string (defined macros), for which I should use pointers
for reading.

A token scanner is represented by the |lex| structure.
Its |size| field serves two purposes: 1) indicating the length of the string
input (when |size>0|), 2) indicating the input is a file (when |size==0|).

(Note: \POSIX/ offers |fmemopen| for creating a |FILE*| given a string;
it's not used in this program.)

Lastly, the token scanner is actually stacked.  Initially, the program
is reading from a file. When it starts to execute a macro, a new token
scanner is pushed onto the stack, and is popped when the macro finishes.
The |link| field indicates the previous token scanner in the stack.

@d LEX_ISFILE(l) ((l)->len == 0)
@s lex int
@c
struct lex {
	union {
		FILE *f;
		char *s;
	} u;
	size_t len; /* not including |'\0'| */
	struct lex *link; /* to form a stack */
};

@ Function |lex_push_file| pushes a token scanner that reads from a file.
@c
struct lex *lex_push_file(struct lex *top, FILE *f)
{
	struct lex *l;

	l = malloc(sizeof *l);
	checkptr(l);
	l->u.f = f;
	l->len = 0;
	l->link = top;
	return l;
}

@ Function |lex_push_str| pushes a token scanner that reads from a string.

If the token scanner has reached the end, we can discard the top before
pushing the new token scanner.  This enables efficient tail recursion.
@c
struct lex *lex_push_str(struct lex *top, char *s, size_t len)
{
	struct lex *l;

	if (top && !LEX_ISFILE(top) && !*top->u.s) {
		l = top;
		free(l->u.s - l->len);
	} else {
		l = malloc(sizeof *l);
		checkptr(l);
		l->link = top;
	}
	l->u.s = s;
	l->len = len;
	return l;
}

@ Function |lex_pop| drops the last token scanner in the stack
and releases its memory.
@c
struct lex *lex_pop(struct lex *top)
{
	struct lex *link;

	assert(top != NULL);
	link = top->link;
	if (!LEX_ISFILE(top))
		free(top->u.s + strlen(top->u.s) - top->len);
		/* ensures the correct pointer when reading halfway */
	free(top);
	return link;
}

@ Function |lex_getc| a character from the input.
A null character is invalid, and if read from a file,
it will be interpreted as end of input.

@c
static int lex_getc(struct lex *l)
{
	int ch;

	if (LEX_ISFILE(l))
		ch = getc(l->u.f);
	else if ((ch = *l->u.s) != '\0')
		++l->u.s; /* don't read pass |'\0'| */
	return ch ? ch : EOF;
}

@ Function |lex_ungetc| pushes back a character to the input.
This is necessary because the token scanner sometimes reads ahead.
Basically, one should only ``unget'' a single character that has just
been read.

@c
static int lex_ungetc(int ch, struct lex *l)
{
	if (LEX_ISFILE(l))
		return ungetc(ch, l->u.f);
	return *--l->u.s = ch;
}

@ Function |lex_gettok| reads a token from the input.
\item{$\bullet$} For a character token, the return value is its character code.
\item{$\bullet$} For a value token, it returns zero, and the value is pushed
onto the main stack.
\item{$\bullet$} In case of end-of-file or end-of-string, it returns |EOF|.

The rules for a value token loosely follows the \.{dc} convention:
the characters that start a value token are \.{[0-9A-F\_.[]}.

@c
struct reg *reg_get(struct reg *, struct lex *); /* defined in later sections */
int lex_gettok(struct lex *l, struct reg **top)
{
	int ch;

	switch (ch = lex_getc(l)) {
	case '0': case '1': case '2': case '3': case '4': @|
	case '5': case '6': case '7': case '8': case '9': @|
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': @|
	case '_': case '.': case '[':
		lex_ungetc(ch, l);
		*top = reg_get(*top, l);
		return 0;
	}
	return ch;
}

@* Registers.
The calculator stores its values in ``registers''.
Registers hold various things, including: integers, floating point numbers
and strings.

We define the |reg| structure to represent the register.
It has a union of different value types, and
an enumeration indicating the type of the value.
Many operations on a register will thus requires a type switch.

Lastly, a register is really a stack of registers, we can push multiple
values onto the register, and pop them out in the reverse order.
We implement this stack by a singly linked list: the |link| field points
to the previous frame in the stack.

@s reg int
@c
struct reg {
	union {
		mpz_t z;
		mpfr_t f;
		struct {
			size_t len; /* not including the trailing |'\0'| */
			char *ptr;
		} s;
	} u;
	enum reg_type {
		REG_Z, REG_F, REG_S
	} type;
	struct reg *link;
};

@ Function |reg_push| pushes an new frame onto the register (stack).

When pushing an integer, the value is initialized to zero.

When pushing a floating point number, the third argument is the precision,
and the value is initialized to NaN (not-a-number).

When pushing a string, the third argument is
the pointer to the first character,
and the fourth argument is
the length of the string (not including |'\0'|).

@c
struct reg *reg_push(struct reg *top, enum reg_type type, ...)
{
	struct reg *r;
	va_list ap;

	r = malloc(sizeof *r);
	checkptr(r);
	va_start(ap, type);
	switch (r->type = type) {
	case REG_Z:
		mpz_init(r->u.z);
		break;
	case REG_F:
		mpfr_init2(r->u.f, va_arg(ap, mp_prec_t));
		break;
	case REG_S:
		r->u.s.ptr = va_arg(ap, char *);
		r->u.s.len = va_arg(ap, size_t);
		break;
	}
	va_end(ap);
	r->link = top;
	return r;
}

@ Function |reg_pop| pops a frame from the register.
@c
struct reg *reg_pop(struct reg *top)
{
	struct reg *link;

	assert(top != NULL);
	link = top->link;
	switch (top->type) {
	case REG_Z:
		mpz_clear(top->u.z);
		break;
	case REG_F:
		mpfr_clear(top->u.f);
		break;
	case REG_S:
		free(top->u.s.ptr);
		break;
	}
	free(top);
	return link;
}

@ Function |reg_dup| makes a copy of a register (the top value on the stack)
and pushes this value to to another register.
@c
struct reg *reg_dup(struct reg *dst, struct reg *src)
{
	switch (src->type) {
	case REG_Z:
		dst = reg_push(dst, REG_Z);
		mpz_set(dst->u.z, src->u.z);
		break;
	case REG_F:
		dst = reg_push(dst, REG_F, mpfr_get_prec(src->u.f));
		mpfr_set(dst->u.f, src->u.f, _rnd_mode);
		break;
	case REG_S:
		dst = reg_push(dst, REG_S,
			memdup(src->u.s.ptr, src->u.s.len + 1),
			src->u.s.len);
		break;
	}
	return dst;
}

@ Function |reg_rot| swaps the register with its previous value.
@c
struct reg *reg_rot(struct reg *top)
{
	struct reg *link;

	assert(top != NULL);
	link = top->link;
	assert(link != NULL);
	top->link = link->link;
	link->link = top;
	return link;
}

@ Function |reg_conv| converts the register to another type.
It is used when the operands have different types, e.g.,
when we want to divide an integer by a floating point number,
we first convert the integer to floating point, and then
perform the calculation.

Currently, the only allowed type to convert to is |REG_F|.
@c
void reg_conv(struct reg *r, enum reg_type type)
{
	mpfr_t f;

	switch (type) {
	case REG_F:
		switch (r->type) {
		case REG_Z:
			mpfr_init2(f, _precision);
			mpfr_set_z(f, r->u.z, _rnd_mode);
			mpz_clear(r->u.z);
			memcpy(r->u.f, f, sizeof f);
			r->type = REG_F;
		case REG_F:
			break;
		default:
			goto no_conv;
		}
		break;
	default:
no_conv:
		assert(0 && "cannot convert");
		break;
	}
}

@ Function |reg_get_ui| converts the value in the register to
an |unsigned long|.
@c
unsigned long reg_get_ui(struct reg *r)
{
	switch (r->type) {
	case REG_Z:
		return mpz_get_ui(r->u.z);
	case REG_F:
		return mpfr_get_ui(r->u.f, _rnd_mode);
	default:
		complain("non-numeric value\n");
		break;
	}
	return 0;
}

@ Function |reg_get_si| converts the value in the register to a |long|.
It complains if the value cannot fit in a |long|.
@c
long reg_get_si(struct reg *r)
{
	switch (r->type) {
	case REG_Z:
		if (!mpz_fits_slong_p(r->u.z))
			complain("invalid integer value\n");
		return mpz_get_si(r->u.z);
	case REG_F:
		if (!mpfr_fits_slong_p(r->u.f, _rnd_mode))
			complain("invalid integer value\n");
		return mpfr_get_si(r->u.f, _rnd_mode);
	default:
		complain("non-numeric value\n");
		break;
	}
	return 0;
}

@*1 Writing registers.
Function |reg_put| writes the register to a file.

When |special=1|, it writes in a different format
(for use with the \.{P} command).
\item{$\bullet$} For an integer, it interprets it as a sequence of bytes
and outputs the strings;
\item{$\bullet$} For a floating point number, it prints it in MPFR's
default format (lowercase letters, always with exponents, and with enough
digits to be read back exactly).

@c
void reg_put(struct reg *r, FILE *f, int special)
{
	switch (r->type) {
	case REG_Z:
		@<Write an integer value@>;
		break;
	case REG_F:
		@<Write a floating point value@>;
		break;
	case REG_S:
		fputs(r->u.s.ptr, f);
		break;
	}
}

@ Writing an integer is easy: |mpz_out_str| does the job.
Function |mpz_out_str| will use uppercase letters if base is negative.

@<Write an integer value@>=
if (!special)
	mpz_out_str(f, -_obase, r->u.z);
else
	@<Write an integer in special format@>;

@ If writing in the special format, we need to find the byte stream
in the ``limbs'' in a |mpz_t| variable, and print them as characters.

@<Write an integer in special format@>=
{
	size_t n;

	if ((n = mpz_size(r->u.z)) == 0)
		putchar('\0');
	else {
		const mp_limb_t *limbs;
		int i;

		limbs = mpz_limbs_read(r->u.z);

#define LIMB_BYTE (limbs[n-1] >> ((i-1)*CHAR_BIT))
		for (i = sizeof (mp_limb_t); i && LIMB_BYTE == 0; i--)
			;
		for (; i; i--)
			putc(LIMB_BYTE & UCHAR_MAX, f);
		while (--n)
			for (i = sizeof (mp_limb_t); i; i--)
				putc(LIMB_BYTE & UCHAR_MAX, f);
#undef LIMB_BYTE
	}
}

@ Writing a floating point number is tricky.
First, there are special values (0, $\pm\infty$ or NaN).
Second, (for normal values,) we need to determine how many digits to output.
Finally, we need to write the decimal point and the exponent properlly.

@<Write a floating point...@>=
{
	if (!mpfr_number_p(r->u.f))
		@<Write a special floating point value@>@;
	else
		@<Write a normal floating point value@>;
}

@ We call |mpfr_out_str| directly for special values.  They will show up
as \.{@@Inf@@}, \.{-@@Inf@@} or \.{@@NaN@@}.
@<Write a special...@>=
mpfr_out_str(f, _obase, 0, r->u.f, _rnd_mode);

@ When writing a normal number, we call |mpfr_get_str| as it gives more
flexibility: we can omit the exponent if it is unnecessary.
However, MPFR does not support a negative base and uses lowercase letters
by default, so we need to convert them to uppercase manually by calling
|toupper|.

@<Write a normal...@>=
{
	size_t n, len, i = 0;
	char *s;
	mpfr_exp_t e;

	@<Determine output digits |n|@>;
	s = mpfr_get_str(NULL, &e, _obase, n, r->u.f, _rnd_mode);
	len = strlen(s);
	@<If negative, output and skip the minus sign@>;
	@<Write digits before decimal point@>;
	putc('.', f); /* writes the decimal point */
	@<Write digits after decimal point@>;
	@<Write the exponent if necessary@>;
	mpfr_free_str(s);
}

@ For a $k$-bit floating point, we output $\lfloor k \log_b 2 \rfloor$ digits
in base~$b$.  The printed result is exact if $k=2$.

If writing in the special format, we will set $n=0$ and let MPFR determine
how many digits to write.
It usually writes too many digits (because it ensures exact result
when read back).  The user may be annoyed if she typed \.{0.2} but found
\.{0.1999...} on the stack.

@<Determine output digits |n|@>=
{
	static const double M[] = {
		.63092975357145743710,
		.50000000000000000000,
		.43067655807339305067,
		.38685280723454158687,
		.35620718710802217651,
		.33333333333333333333,
		.31546487678572871855,
		.30102999566398119521,
		.28906482631788785927,
		.27894294565112984319,
		.27023815442731974129,
		.26264953503719354798,
		.25595802480981548939,
		.25000000000000000000
	}; /* |M[i]|${}=1/\log_2(i+3)$ */

	if (special)
		n = 0; /* let MPFR decide */
	else {
		n = mpfr_get_prec(r->u.f);
		if (_obase > 2)
			n *= M[_obase - 3];
		if (n == 0)
			n = 1; /* must output at least one digit! */
	}
}

@ @<If negative...@>=
if (s[0] == '-') {
	putc('-', f);
	++i;
	--len;
}

@ We will omit the exponent if |e| is between 0 and |len|.
In this case, we need to place the decimal point at a proper place,
i.e., after the |e|-th digit.

@<Write digits before...@>=
if (!special && !mpfr_zero_p(r->u.f) && 0 <= e && (size_t) e <= len)
	/* don't need an exponent */
	for (; e; e--) /* |e| digits before the decimal point */
		putc(toupper(s[i++]), f);
else /* need an exponent */
	putc(s[i++], f); /* only 1 digit before the decimal point */

@ @<Write digits after...@>=
if (!special)
	while (s[len-1] == '0')
		--len; /* remove trailing zeros */
while (i < len)
	putc(toupper(s[i++]), f);

@ We use \.{e} to indicate an exponent.
The exponent is always written in decimal.

Note: If we don't need an exponent, |e| was set to zero in
|@<Write digits before...@>|.

@<Write the exponent...@>=
	if (e)
		fprintf(f, "e%ld", (long) (e-1));

@*1 Reading registers.
As pointed out in the ``Token scanner'' section, the input comes from
two sources.  Therefore, we need the token scanner to read into a register.

A buffer is used here for input with unknown length.

@c
struct reg *reg_get(struct reg *r, struct lex *l)
{
	char *buf;
	size_t len = 0, cap = 8;
	int ch;

	buf = malloc(cap);
	checkptr(buf);
	ch = lex_getc(l);
	if (ch == '[')
		@<Read a string token@>@;
	else
		@<Read a numeric token@>;
	return r;
}

@ A string token is a character sequenced enclosed by a balanced pair of
brackets.  It's unfortunate that there is no escaping so one cannot have
a string containing unbalanced brackets. (Bad for the user, good for the
implementer, though.)

@<Read a string token@>=
{
	int balance = 1;

	for (;;) {
		ch = lex_getc(l);
		switch (ch) {
		case '[': ++balance; break;
		case ']': --balance; break;
		case EOF: balance = 0;
			complain("unbalanced []\n");
			break;
		}
		if (balance == 0)
			break;
		buf = buf_add(buf, &len, &cap, ch);
	}
	@<Terminate the buffer@>;
	r = reg_push(r, REG_S, buf, len);
}

@ @<Terminate the buffer@>=
buf = realloc(buf, len+1);
checkptr(buf);
buf[len] = '\0';

@ A numeric token contains an optional \.{\_} (negative sign),
followed by a sequence of hexadecimal digits with an optional decimal point.
This is the \.{dc} convention.  The extension here is to allow an optional
expoent in the end, indicated by character \.{e} or \.{@@}.

Note: \.{dc} allowed all of \.{[0-9A-F]} to be valid digits in any base.
GMP and MPFR do not allow this, so here is the compromise:
\item{$\bullet$} Any one-digit number is a hexadecimal integer, no matter
what the current input base is.
\item{$\bullet$} In any other case, the input digits must contains digits
that are meaningful in the input base; otherwise the program will complain.

@<Read a numeric token@>=
{
	int isint = 1;

	@<Read an optional negative sign@>;
	@<Scan hexdigit sequence@>;
	@<Read an optional fractional part@>;
	@<Read an optional exponent@>;
	lex_ungetc(ch, l); /* push back the read-ahead */
	@<Terminate the buffer@>;
	@<Push the numeric value onto the stack@>;
	free(buf);
}

@ We use \.{\_} for negative sign, but GMP and MPFR use \.{-}.
@<Read an optional negative sign@>=
if (ch == '_') {
	ch = '-';
	@<Add |ch| to |buf| and get next |ch|@>;
}

@ If we see a decimal point, a fractional part follows.
@<Read an optional fractional part@>=
if (ch == '.') {
	isint = 0;
	@<Add |ch| to |buf|...@>;
	@<Scan hexdigit sequence@>;
}

@ If we see an \.{e}, an exponent follows.
@<Read an optional exponent@>=
if (ch == 'e') {
	isint = 0;
	ch = '@@'; /* don't confuse MPFR when |_ibase>10| */
	@<Add |ch| to |buf|...@>;
	if (ch == '+' || ch == '-')
		@<Add |ch| to |buf|...@>;
	@<Scan hexdigit sequence@>;
}

@ @<Scan hexdigit sequence@>=
while (isdigit(ch) || (isxdigit(ch) && isupper(ch)))
	@<Add |ch| to |buf|...@>;

@ @<Add |ch| to |buf|...@>=
{
	buf = buf_add(buf, &len, &cap, ch);
	ch = lex_getc(l);
}

@ @<Push the numeric value...@>=
{
	int rc;
	if (isint) {
		r = reg_push(r, REG_Z);
		rc = mpz_set_str(r->u.z, buf, (len == 1) ? 16 : _ibase);
	} else {
		r = reg_push(r, REG_F, _precision);
		rc = mpfr_set_str(r->u.f, buf, _ibase, _rnd_mode);
	}
	if (rc == -1)
		complain("malformed number %s in base %d\n", buf, _ibase);
}

@*1 Unary operations.  
Function |reg_un| performs a type-generic unary operation.
The top of the stack is the input and will be rewritten with the result.

@c
struct reg *reg_un(struct reg *r,
	@|void @[(*fz)@](mpz_t, const mpz_t),
	@|int @[(*ff)@](mpfr_t, const mpfr_t, mpfr_rnd_t))
{
	if (r == NULL) {
		complain("stack empty\n");
		return r;
	}
	switch (r->type) {
	case REG_Z:
		if (fz != NULL) {
			(*fz)(r->u.z, r->u.z);
			break;
		}
		reg_conv(r, REG_F);
		/* fall through */
	case REG_F:
		if (ff == NULL)
			complain("operation not supported\n");
		else
			(*ff)(r->u.f, r->u.f, _rnd_mode);
		break;
	default:
		complain("non-numeric value\n");
		break;
	}
	return r;
}

@*1 Binary operations.
This program implements the binary operations from \.{dc}
as well as some extensions.
When executed, two values are popped from the stack, and the result
is pushed back.

Function |reg_bin| performs a type-generic binary operation.
Two values are popped from the stack and the result is pushed back.

When two operands have different types, we will try to match them with
mixed-type functions that exists in GMP and MPFR.
If such functions are not available, we convert them to a common type
(|REG_F|), and then perform the calculation.

@c
struct reg *reg_bin(struct reg *r,
	@|void @[(*fzz)@](mpz_t, const mpz_t, const mpz_t),
	@|int @[(*fzf)@](mpfr_t, const mpfr_t, const mpz_t, mpfr_rnd_t),
	@|int @[(*ffz)@](mpfr_t, const mpfr_t, const mpz_t, mpfr_rnd_t),
	@|int @[(*fff)@](mpfr_t, const mpfr_t, const mpfr_t, mpfr_rnd_t))
{
	struct reg *l;
	mpfr_t tmp;

	if (r == NULL || (l = r->link) == NULL) {
		complain("stack empty\n");
		return r;
	}
reswitch:
	switch (l->type) {
	case REG_Z:
		switch (r->type) {
		case REG_Z:
			@<Binary operations on integers@>;
			break;
		case REG_F:
			@<Binary operations on integer and floating point@>;
			break;
		default:
			goto non_numeric;
		}

		break;
	case REG_F:
		switch (r->type) {
		case REG_Z:
			@<Binary operations on floating point and integer@>;
			break;
		case REG_F:
			@<Binary operations on floating points@>;
			break;
		default:
			goto non_numeric;
		}
		break;
	default:
non_numeric:
		complain("non_numeric value\n");
		return r;
	}
	return reg_pop(r);
not_supported:
	complain("operation not supported\n");
	return r;
}

@ When two operands are integer, call {\it mpz\_*} functions.

We can reuse |l| for the result.

@<Binary operations on integers@>=
if (fzz == NULL)
	goto not_supported;
(*fzz)(l->u.z, l->u.z, r->u.z);

@ When |l| is integer and |r| is floating point
and a symmetric function is available,
swap |l| and |r| and reduce to the symmetric case.

Otherwise, convert |l| to floating point and retry;

@<Binary operations on integer and floating point@>=
{
	if (fzf == NULL)
		reg_conv(l, REG_F);
	else {
		r = reg_rot(r);
		assert(l == r);
		l = r->link;
		ffz = fzf;
	}
	goto reswitch;
}

@ When |l| is floating point and |r| is integer, try
{\it mpfr\_*\_z} functions.
If no function is available, convert |r| to floating point and retry.

We can reuse |l| for the result.  However, we should always create
a new |mpfr_t| value because the precision may differ.

@<Binary operations on floating point and integer@>=
if (ffz == NULL) {
	reg_conv(r, REG_F);
	goto reswitch;
}
mpfr_init2(tmp, _precision);
(*ffz)(tmp, l->u.f, r->u.z, _rnd_mode);
goto tmp_swap_clear;

@ When two operands are floating point, call {\it mpfr\_*} functions.

@<Binary operations on floating points@>=
if (fff == NULL)
	goto not_supported;
mpfr_init2(tmp, _precision);
(*fff)(tmp, l->u.f, r->u.f, _rnd_mode);
tmp_swap_clear:
mpfr_swap(tmp, l->u.f);
mpfr_clear(tmp);


@*1 Comparisons.  Comparizons are similar to binary operations except
that no values are popped or pushed.

Function |reg_cmp| performs a comparison.
The return value has the same sign as the expression $l-r$.
If one of the operand is a string or NaN, the return value is zero.

@c
int reg_cmp(struct reg *l, struct reg *r)
{
	assert(l != NULL && r != NULL);
	switch (l->type) {
	case REG_Z:
		switch (r->type) {
		case REG_Z:
			return mpz_cmp(l->u.z, r->u.z);
		case REG_F:
			return -mpfr_cmp_z(r->u.f, l->u.z);
		default:
			goto non_numeric;
		}
		break;
	case REG_F:
		switch (r->type) {
		case REG_Z:
			return mpfr_cmp_z(l->u.f, r->u.z);
		case REG_F:
			return mpfr_cmp(l->u.f, r->u.f);
		default:
			goto non_numeric;
		}
		break;
	default:
non_numeric:
		complain("non-numeric value\n");
		break;
	}
	return 0;
}

@* Interpreter.  The input is interpreted as tokens are read.
The interpreter can manipulate the stack, the registers, and the token scanner.

Function |interpret| reads tokens from the token scanner, executes them
until no more tokens are available.

@c
@<Auxiliary functions for the interpreter@>@;
void interpret(struct lex *l, struct reg *r[])
{
	int tok;
	int i;

	while (l != NULL) {
		switch (tok = lex_gettok(l, &r[0])) {
		@<Actions for input token@>;
		@<Interpreter error conditions@>;
		}
	}
}

@ When the input token is a space character or |'\0'|
(a value token has been read), no action is required.
@<Actions...@>+=
case ' ': case '\t': case '\v': case '\f':
case '\r': case '\0':
	break; /* no action */

@ @<Interpreter error conditions@>=
default:
	complain("%s not implemented\n", tok2str(tok));
	break;
stack_empty:
	complain("stack empty\n");
	break;

@*1 Stack manipulation.  The commands in this section operates on
the main stack.
The main stack is actually register~0. It cannot be accessed by
commands that requires a register name because |'\0'| is not
a character in the input.  (Note: this is different from \.{dc}).

@ The \.{c} command clears the stack.
@<Actions...@>+=
case 'c':
	while (r[0])
		r[0] = reg_pop(r[0]);
	break;

@ The \.{d} command duplicates the stack top.
@<Actions...@>+=
case 'd':
	if (r[0] == NULL)
		goto stack_empty;
	r[0] = reg_dup(r[0], r[0]);
	break;

@ The \.{f} command dumps the entire stack (from top to bottom).
@<Actions...@>+=
case 'f':
{
	struct reg *p;

	for (p = r[0]; p != NULL; p = p->link) {
		reg_put(p, stdout, 0);
		fputc('\n', stdout);
	}
	break;
}

@ The \.{p}, \.{P} and \.{n} commands print the top of the stack.
The differences are:
\item{$\bullet$} \.{p} appends a newline whereas \.{P} and \.{n} do not.
\item{$\bullet$} \.{p} does not pop the value whereas \.{P} and \.{n} do.
\item{$\bullet$} \.{P} uses a different format.

@<Actions...@>+=
case 'n':
case 'p':
case 'P':
	if (r[0] == NULL)
		goto stack_empty;
	reg_put(r[0], stdout, tok == 'P');
	if (tok == 'p')
		fputc('\n', stdout);
	else
		r[0] = reg_pop(r[0]);
	break;

@ The \.{r} command swaps the top of the stack and the second of the
stack.
@<Actions...@>+=
case 'r':
	if (r[0] == NULL || r[0]->link == NULL)
		goto stack_empty;
	r[0] = reg_rot(r[0]);
	break;

@ The \.{z} command pushes the depth of the stack onto the stack.
@<Actions...@>+=
case 'z':
{
	unsigned long depth = 0;
	struct reg *p;

	for (p = r[0]; p; p = p->link)
		++depth;
	r[0] = reg_push(r[0], REG_Z);
	mpz_set_ui(r[0]->u.z, depth);
	break;
}

@*1 Control flow.  The commands in this section make changes to the token
scanner, so we do not always execute the input linearly.

@ On end-of-input, the current token scanner is popped.
The \.{q} command pops two levels of the token scanners.

@<Actions...@>+=
case 'q':
	l = lex_pop(l);
	if (l == NULL)
		break;
	/* fall through */
case EOF:
	l = lex_pop(l);
	break;

@ The \.{Q} command pops a value $n$ from the main stack
and pops $n$ token scanners.  It never pops the last one, though.
@<Actions...@>+=
case 'Q':
{
	size_t n;

	if (r[0] == NULL)
		goto stack_empty;
	n = reg_get_ui(r[0]);
	r[0] = reg_pop(r[0]);
	while (n-- && l->link)
		l = lex_pop(l);
	break;
}

@ The \.{?} command reads a line from standard input and executes it.
@<Actions...@>+=
case '?':
	l = lex_push_file(l, stdin);
	break;

@ On a newline character, if the current input is a file and is not the
last level (i.e., we are in a \.{?} command), quit the current level.
@<Actions...@>+=
case '\n':
	if (LEX_ISFILE(l) && l->link != NULL)
		l = lex_pop(l);
	break;

@ The \.{x} command takes the top of the stack and executes it.
@<Actions...@>+=
case 'x':
	if (r[0] == NULL)
		goto stack_empty;
	if (r[0]->type == REG_S) {
		l = lex_push_str(l, r[0]->u.s.ptr, r[0]->u.s.len);
		r[0]->u.s.ptr = NULL; /* steal the string */
		r[0] = reg_pop(r[0]);
	}
	/* otherwise the value is pushed back */
	break;

@ The \.{!} command reads a line and invokes an external command
as indicated by the line.

@<Actions...@>+=
case '!':
{
	char *buf;
	size_t len = 0, cap = 64;
	int ch;

	buf = malloc(cap);
	checkptr(buf);
	while ((ch = lex_getc(l)) != EOF && ch != '\n')
		buf = buf_add(buf, &len, &cap, ch);
	buf = buf_add(buf, &len, &cap, '\0');
	if (system(buf) == -1)
		perror(progname);
	free(buf);
	break;
}

@ The \.{\#} command skips tokens until the end of the line.
It is usually used for comments.

@<Actions...@>+=
case '#':
	while ((tok = lex_getc(l)) != EOF && tok != '\n')
		;
	break;

@ The conditional commands (\.{<}$x$, \.{>}$x$ and \.{=}$x$)
executes macro~$x$ if the top of the stack is smaller than\slash
greater than\slash equal to the second of the stack.

Note: |reg_cmp| returns zero if one of the operands is NaN.
Obviously, \.{=}$x$ should not execute $x$, so we must check
the {\tt ERANGE} flag in MPFR.

@<Actions...@>+=
case '<':
case '>':
case '=':
{
	struct reg *lhs, *rhs;
	int rc;

	mpfr_clear_erangeflag();
	if ((lhs = r[0]) == NULL || (rhs = lhs->link) == NULL) {
		complain("stack empty\n");
		rc = 0;
	} else {
		rc = reg_cmp(lhs, rhs);
		r[0] = reg_pop(reg_pop(r[0]));
	}
	if ((i = get_reg(l, r, 0)) == -1)
		break;
	if (!mpfr_erangeflag_p() &&
			((tok == '<' && rc < 0) ||
			(tok == '=' && rc == 0) ||
			(tok == '>' && rc > 0))) {
		if (r[i] == NULL)
			complain("register %s is empty\n", tok2str(i));
		else if (r[i]->type == REG_S)
			l = lex_push_str(l,
				memdup(r[i]->u.s.ptr, r[i]->u.s.len + 1),
				r[i]->u.s.len);
		else
			r[0] = reg_dup(r[0], r[i]);
	}
	break;
}

@ Function |get_reg| reads a character from the input and use its
character code (ranging from 0 to |UCHAR_MAX+1|) to determine the
register.  On most common machines, there are 256 registers.

It returns the index to the register, or $-1$ if something goes wrong.

@<Auxiliary functions for the interpreter@>+=
int get_reg(struct lex *l, struct reg *r[], int non_empty)
{
	int i;

	i = lex_getc(l);
	if (i == EOF) {
		complain("unexpected EO%c\n", LEX_ISFILE(l) ? 'F' : 'S');
		return -1;
	}
	i &= UCHAR_MAX;
	if (non_empty && r[i] == NULL) {
		complain("register %s is empty\n", tok2str(i));
		return -1;
	}
	return i;
}

@*1 Register operations.  The commands in this section operates on
the registers.  Values can be transferred between registers and the main
stack.  Note that each register is itself a stack as well.

@ The \.{s}~command saves the top of the stack to a register, the
register's previous value is discarded.
The \.{S}~command saves the top of the stack to a register, the
register's previous value can be restored by the \.{L}~command.

@<Actions...@>+=
case 's':
case 'S':
{
	struct reg *top;

	if ((i = get_reg(l, r, 0)) != -1) {
		if ((top = r[0]) == NULL)
			goto stack_empty;
		r[0] = r[0]->link;
		top->link = (tok == 'S') ? r[i] :
			(r[i]) ? reg_pop(r[i]) : NULL;
		r[i] = top;
	}
	break;
}

@ The \.{l}~command loads the value in a register and pushes it onto
the stack.  The value of the register keeps unchanged.

@<Actions...@>+=
case 'l':
	if ((i = get_reg(l, r, 1)) != -1)
		r[0] = reg_dup(r[0], r[i]);
	break;

@ The \.{L}~command loads the value in a register and pushes it onto
the stack.  The register is restored to its previous value.

@<Actions...@>+=
case 'L':
{
	struct reg *link;

	if ((i = get_reg(l, r, 1)) != -1) {
		link = r[i]->link;
		r[i]->link = r[0];
		r[0] = r[i];
		r[i] = link;
	}
	break;
}

@*1 Binary operators.
The commands in this section performs binary operations.

Let $y$ be the first popped value and $x$ the second.
The binary operations are summarized as follows.
$$\tabskip=1em\vbox{\halign{
\strut\tt\hfil#\hfil&\hfil$#$\hfil&#\hfil\cr
\noalign{\hrule\kern1pt}
\rm Operator&\hbox{Result}&Notes\cr
\noalign{\hrule\kern1pt}
 +&  x+y\cr
 -&  x-y\cr
 *&  x\times y\cr
 /&  x\div y&Result is truncated if both are integers\cr
\%&  x\bmod y\cr
\^{}&x^y\cr
\noalign{\hrule}
}}$$

@<Actions...@>+=
case '+':
	r[0] = reg_bin(r[0], mpz_add, mpfr_add_z, mpfr_add_z, mpfr_add);
	break;
case '-':
	r[0] = reg_bin(r[0], mpz_sub, my_mpfr_z_sub, mpfr_sub_z, mpfr_sub);
	break;
case '*':
	r[0] = reg_bin(r[0], mpz_mul, mpfr_mul_z, mpfr_mul_z, mpfr_mul);
	break;
case '^':
	r[0] = reg_bin(r[0], my_mpz_pow, NULL, mpfr_pow_z, mpfr_pow);
	break;

@ Function |my_mpz_pow| takes two |mpz_t|s but actually only works
if the exponent fits in an |unsigned long|.

@<Auxiliary functions for the interpreter@>+=
void my_mpz_pow(mpz_t z, const mpz_t x, const mpz_t y)
{
	if (!mpz_fits_ulong_p(y)) {
		complain("infeasible exponent\n");
		return;
	}
	mpz_pow_ui(z, x, mpz_get_ui(y));
}

@ Function |my_mpfr_z_sub| is the symmetric version of |mpfr_sub_z|.
It is necessary because 1) subtraction is not commutative;
2) we need to match the function prototype.

@<Auxiliary functions for the interpreter@>+=
int my_mpfr_z_sub(mpfr_t z, const mpfr_t x, const mpz_t y, mpfr_rnd_t r)
{
	return mpfr_z_sub(z, y, x, r);
}

@ Division requires some caution to avoid division by zero.
Only integer division is dangerous; floating point division-by-zero
results in NaN.

@d REG_IS_Z(r) ((r)&&(r)->type == REG_Z)
@<Actions...@>+=
case '/':
case '%':
	if (REG_IS_Z(r[0]) && REG_IS_Z(r[0]->link) &&
			mpz_cmp_ui(r[0]->u.z, 0) == 0) {
		complain("division by zero\n");
		break;
	}
	if (tok == '/')
		r[0] = reg_bin(r[0], mpz_tdiv_q, NULL, mpfr_div_z, mpfr_div);
	else
		r[0] = reg_bin(r[0], mpz_tdiv_r, NULL, NULL, mpfr_fmod);
	break;

@ Command \.{\~} takes two integers from the stack and pushes
the quotient and remainder back.  It works only for integers.

@<Actions...@>+=
case '~':
{
	struct reg *lhs, *rhs;

	if ((rhs = r[0]) == NULL || (lhs = rhs->link) == NULL)
		goto stack_empty;
	if (lhs->type != REG_Z && rhs->type != REG_Z)
		complain("non-integer value\n");
	else
		mpz_tdiv_qr(lhs->u.z, rhs->u.z, lhs->u.z, rhs->u.z);
	break;
}

@ Command \.{\char"7C} is actually a ternary operator.
It takes three integers $x$, $y$, $m$ and returns $x^y \bmod m$
(borrowed from GNU \.{dc}).
It can be useful if $y$ is very large.

@<Actions...@>+=
case '|':
{
	struct reg *x, *y, *m;

	if ((m = r[0]) == NULL || (y = m->link) == NULL ||
			(x = y->link) == NULL)
		goto stack_empty;
	if (x->type != REG_Z || y->type != REG_Z || m->type != REG_Z)
		complain("non-integer value\n");
	else if (mpz_cmp_ui(y->u.z, 0) == 0 || mpz_cmp_ui(m->u.z, 0) == 0)
		complain("division by zero\n");
	else {
		mpz_powm(x->u.z, x->u.z, y->u.z, m->u.z);
		r[0] = reg_pop(reg_pop(r[0]));
	}
	break;
}

@*1 Unary operators.
Unary operations are mostly mathematical functions.
Let $x$ be the popped value.
The unary operations are summarized as follows.
$$\tabskip=1em\vbox{\halign{
\strut\tt\hfil#\hfil&\hfil$#$\hfil&#\hfil\cr
\noalign{\hrule\kern1pt}
\rm Operator&\hbox{Result}&Notes\cr
\noalign{\hrule\kern1pt}
v&  \sqrt x&Inherited from \.{dc}; result is truncated if $x$ is integer\cr
s&  \sin x&Borrowed from \.{bc}\cr
c&  \cos x&Borrowed from \.{bc}\cr
a&  \arctan x&Borrowed from \.{bc}\cr
l&  \ln x&Borrowed from \.{bc}\cr
e&  \exp x&Borrowed from \.{bc}\cr
z&  \zeta(x)&Extension; $\zeta(x)=\sum_{n=1}^\infty {1\over n^x}$\cr
g&  \Gamma(x)&Extension; $\Gamma(x)=\int_0^\infty t^{x-1}e^{-t}\;dt$\cr
G&  \ln\Gamma(x)&Extension\cr
r&  x\hbox{ rounded to integer}&Extension\cr
\noalign{\kern1pt\hrule}
}}$$
Note: only \.{v} is a dedicated command.  Other operations must be preceded
by \.{m}.

@<Actions...@>+=
case 'v':
	if (REG_IS_Z(r[0]) && mpz_cmp_ui(r[0]->u.z, 0) < 0)
		complain("square root of negative number\n");
	else
		r[0] = reg_un(r[0], mpz_sqrt, mpfr_sqrt);
	break;
case 'm':
	switch (tok = get_reg(l, r, 0)) {
	case -1: break; /* invalid */
	@<Math library functions@>;
	default:
		complain("math function %s not implmented\n", tok2str(tok));
		break;
	}
	break;

@ For unary math functions, call |reg_un|.
@<Math library functions@>=
case 's': r[0] = reg_un(r[0], NULL, mpfr_sin); break;
case 'c': r[0] = reg_un(r[0], NULL, mpfr_cos); break;
case 'a': r[0] = reg_un(r[0], NULL, mpfr_atan); break;
case 'l': r[0] = reg_un(r[0], NULL, mpfr_log); break;
case 'e': r[0] = reg_un(r[0], NULL, mpfr_exp); break;
case 'z': r[0] = reg_un(r[0], NULL, mpfr_zeta); break;
case 'g': r[0] = reg_un(r[0], NULL, mpfr_gamma); break;
case 'G': r[0] = reg_un(r[0], NULL, mpfr_lngamma); break;
case 'r': r[0] = reg_un(r[0], NULL, mpfr_rint); break;

@*1 Special functions.

\.{j} and \.{y} are the two kinds of the Bessel functions.
$$\eqalign{
J_n(x) &= \sum_{m=0}^\infty {(-1)^m\over m!\,(m+n)!}
                     \left(x\over2\right)^{2m+n},\cr
Y_n(x) &= {J_n(x)\cos n\pi - J_{-n}(x) \over \sin n\pi}.
}$$
They need to be preceded by \.{m}, and
only integer orders are supported due to the MPFR implementation.

Note: \.{bc} supports only \.{j} and also only integer orders.

@<Math library functions@>+=
case 'j':
case 'y':
{
	struct reg *n, *x;

	if ((n = r[0]) == NULL || (x = n->link) == NULL)
		goto stack_empty;
	if (n->type == REG_S || x->type == REG_S) {
		complain("non-numeric value\n");
		break;
	}
	reg_conv(x, REG_F);
	((tok == 'j') ? mpfr_jn : mpfr_yn)
		(x->u.f, reg_get_si(n), x->u.f, _rnd_mode);
	r[0] = reg_pop(r[0]);
	break;
}

@*1 Parameters.  The parameters in the global state can be changed
by the commands in this section.

@ The \.{i} and \.{o} commands set the input and output base, respectively.
@<Actions...@>+=
case 'i':
case 'o':
{
	unsigned long b;

	if (r[0] == NULL)
		goto stack_empty;

	b = reg_get_ui(r[0]);

	if (b < 2)
		complain("base too small; set to %d\n", b=2);
	else if (b > 16)
		complain("base too large; set to %d\n", b=16);
	if (tok == 'i')
		_ibase = b;
	else
		_obase = b;
	r[0] = reg_pop(r[0]);
	break;
}

@ The \.{I} and \.{O} commands push the current input and output base onto
the stack, respectively.
@<Actions...@>+=
case 'I':
case 'O':
	r[0] = reg_push(r[0], REG_Z);
	mpz_set_si(r[0]->u.z, (tok == 'I') ? _ibase : _obase);
	break;

@ The \.{k} command sets the floating point precision.

Note: the precision is measured in bits, not decimal places.
This is different from \.{dc}.
Furthermore, the precision does not affect integer arithmetics.

@<Actions...@>+=
case 'k':
{
	unsigned long p;

	if (r[0] == NULL)
		goto stack_empty;
	p = reg_get_ui(r[0]);
	if (p < MPFR_PREC_MIN)
		complain("precision too small; set to %ld\n",
			(long) (p = MPFR_PREC_MIN));
	else if (p > MPFR_PREC_MAX)
		complain("precision too large; set to %ld\n",
			(long) (p = MPFR_PREC_MAX));
	_precision = p;
	r[0] = reg_pop(r[0]);
	break;
}

@ The \.{K} command pushes the current floating point precision onto the stack.
@<Actions...@>+=
case 'K':
	r[0] = reg_push(r[0], REG_Z);
	mpz_set_ui(r[0]->u.z, _precision);
	break;

@ The \.{u} command sets the floating point rounding mode.
@<Actions...@>+=
case 'u':
	if (r[0] == NULL)
		goto stack_empty;
	switch (reg_get_si(r[0])) {
	case 0: _rnd_mode = MPFR_RNDN; break;
	case 1: _rnd_mode = MPFR_RNDZ; break;
	case 2: _rnd_mode = MPFR_RNDU; break;
	case 3: _rnd_mode = MPFR_RNDD; break;
	case 4: _rnd_mode = MPFR_RNDA; break;
	default: complain("invalid rounding mode\n"); break;
	}
	r[0] = reg_pop(r[0]);
	break;

@ The \.{U} command pushes the current floating point precision onto the stack.
@<Actions...@>+=
case 'U':
	r[0] = reg_push(r[0], REG_Z);
	mpz_set_ui(r[0]->u.z, _rnd_mode);
	break;


@* Main program.
The main program mimics the logic of \.{dc}.
It executes each file in the command line arguments,
and if there is none, and \.{-e} options is not present,
then the standard input is read.

@p
@<Auxiliary functions for |main|@>@;
int main(int argc, char *argv[])
{
	static struct reg *r[UCHAR_MAX+1];
	int rc = 0;
	int read_stdin = 1;

	@<Process command line arguments@>;

	if (*argv != NULL)
		do
			exec_file(fopen(*argv++, "r"), r);
		while (*argv != NULL);
	else if (read_stdin)
		exec_file(stdin, r);

	@<Clean up registers@>;
	return rc;
}

@ Function |exec_file| executes a file.
@<Auxiliary functions for |main|@>=
void exec_file(FILE *f, struct reg *r[])
{
	if (f == NULL)
		perror(progname);
	else {
		interpret(lex_push_file(NULL, f), r);
		fclose(f);
	}
}


@ @<Process command line arguments@>=
progname = *argv++;

while (*argv && **argv == '-') {
	int ch;
	switch (ch = (*argv)[1]) {
	@<Command line options@>;
	}
}

@ @<Command line options@>+=
case 'e':
case 'f':
{
	char *arg;
	size_t len;

	arg = (*argv++) + 2;
	if (*arg == '\0' && (arg = *argv++) == NULL) {
		complain("missing argument for %s\n", argv[-1]);
		return 1;
	}
	if (ch == 'e') {
		read_stdin = 0;
		len = strlen(arg);
		interpret(lex_push_str(NULL, memdup(arg, len+1), len), r);
	} else {
		exec_file(fopen(arg, "r"), r);
	}
	break;
}

@ @<Command line options@>+=
default:
	complain("unknown option %s\n", *argv);
	rc = 1;
	/* fall through */
case 'h':
	printf("Usage: %s [-efh] [file...]\n"@|
		"  -e expr   execute expr\n"@|
		"  -f file   execute file\n"@|
		"  -h        print help\n",
		progname);
	return rc;

@ @<Clean up registers@>=
{
	int i;
	for (i = 0; i <= UCHAR_MAX; i++)
		while (r[i])
			r[i] = reg_pop(r[i]);
}

@* Index.
