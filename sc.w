
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
@s mp_limb_t int

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

@ For error reporting purpose, we convert a character token into the
string form.  If the character is printable, the string contains
the character itself and its character code (in octal).
Otherwise, only the character code is included.

@c
const char *tok2str(int tok)
{
	static char buf[20];

	assert(0 <= tok && tok <= UCHAR_MAX);
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


@* Types and values.
There are three data types in this program:
\item{$\bullet$} Integers;
\item{$\bullet$} Floating-point (FP) numbers;
\item{$\bullet$} Strings;

The values are stored in the |val| structure, which is a reference-counted
object.  Reference counting helps reduce unnecessary copies while executing
the program.

@s val int
@c
struct val {
	size_t refcnt;
	enum val_type {
		V_INT, V_FP, V_STR
	} type;
	union {
		mpz_t z;
		mpfr_t f;
		struct {
			size_t len; /* excluding trailing |'\0'| */
			char *ptr;
		} s;
	} u;
};

@ Function |val_ref| increments the reference count.
The reference count is only incremented when the value is inserted into
other structures (stack, array, etc.).

@c
struct val *val_ref(struct val *v)
{
	++v->refcnt;
	return v;
}

@ Function |val_deref| decrements when removed from
other strucutres.
If the only reference to the object is a local (temporary) variable,
the reference count is zero and the object is considered ``floating''.
@c
void val_deref(struct val *v)
{
	assert(v->refcnt);
	--v->refcnt;
}

@ If the object is potentially floating,
one must call |val_ckref| before losing reference to that object
to avoid resources leak.

We must be careful about aliasing, though.
If |x| and |y| points to the same floating object, then
sequentially checking for floating results in undefined behavior.

The convenience macro |val_unref| is provided to perform the checking after
decrementing the reference count.

@d val_unref(v) (val_deref(v), val_ckref(v))
@d val_ckref2(x, y) do { val_ckref(x); if (x != y) val_ckref(y); } while (0)
@d val_ckref3(x, y, z) do { val_ckref2(x, y);
	if (x != z && y != z) val_ckref(z); } while (0)
@c
size_t val_ckref(struct val *v)
{
	if (v->refcnt)
		return v->refcnt;

	switch (v->type) {
	case V_INT:
		mpz_clear(v->u.z);
		break;
	case V_FP:
		mpfr_clear(v->u.f);
		break;
	case V_STR:
		free(v->u.s.ptr);
		break;
	}
	free(v);
	return 0;
}

@ Function |new_val| creates a new value of the given type.
The value is uninitialized and is initially floating.

@c
struct val *new_val(enum val_type type)
{
	struct val *v;

	v = malloc(sizeof *v);
	checkptr(v);
	v->refcnt = 0;
	v->type = type;
	return v;
}

@ Function |new_int| creates an initialized integer value.
@c
struct val *new_int(void)
{
	struct val *v;

	v = new_val(V_INT);
	mpz_init(v->u.z);
	return v;
}

@ Function |new_fp| creates an initialized floating point value.
@c
struct val *new_fp(mpfr_prec_t prec)
{
	struct val *v;

	v = new_val(V_FP);
	mpfr_init2(v->u.f, _precision);
	return v;
}

@ Function |new_str| creates a string value initialized to the given string.
The ownership is transferred.
@c
struct val *new_str(char *s, size_t len)
{
	struct val *v;

	v = new_val(V_STR);
	v->u.s.ptr = s;
	v->u.s.len = len;
	return v;
}

@*1 Conversions.
Function |val_to_ui| converts the value to an |unsigned long|.
The return value indicates whether the operation succeeded.

@c
int val_to_ui(struct val *v, unsigned long *x)
{
	switch (v->type) {
	case V_INT:
		*x = mpz_get_ui(v->u.z);
		return mpz_fits_ulong_p(v->u.z);
	case V_FP:
		*x = mpfr_get_ui(v->u.f, _rnd_mode);
		return mpfr_fits_ulong_p(v->u.f, _rnd_mode);
	default:
		complain("non-numeric value\n");
		*x = 0;
		return 0;
	}
}

@ Similarly, function |val_to_si| converts the value to a |long|.
@c
int val_to_si(struct val *v, long *x)
{
	switch (v->type) {
	case V_INT:
		*x = mpz_get_si(v->u.z);
		return mpz_fits_slong_p(v->u.z);
	case V_FP:
		*x = mpfr_get_si(v->u.f, _rnd_mode);
		return mpfr_fits_slong_p(v->u.f, _rnd_mode);
	default:
		complain("non-numeric value\n");
		*x = 0;
		return 0;
	}
}

@ Function |val_to_fp| converts the value to floating point.
It is used when the operands have different types.

@c
struct val *val_to_fp(struct val *v)
{
	mpfr_t f;

	switch (v->type) {
	case V_INT:
		mpfr_init2(f, _precision);
		mpfr_set_z(f, v->u.z, _rnd_mode);
		if (v->refcnt) {
			v = new_val(V_FP);
		} else {
			mpz_clear(v->u.z);
			v->type = V_FP;
		}
		memcpy(v->u.f, f, sizeof f);
	case V_FP:
		break;
	default:
		return NULL;
	}
	return v;
}

@*1 Comparisons.
Function |val_cmp| performs a comparison.
The return value has the same sign as the expression $l-r$.
If one of the operand is a string or NaN, the return value is zero.

@c
int val_cmp(struct val *l, struct val *r)
{
	assert(l != NULL && r != NULL);
	switch (l->type) {
	case V_INT:
		switch (r->type) {
		case V_INT:
			return mpz_cmp(l->u.z, r->u.z);
		case V_FP:
			return -mpfr_cmp_z(r->u.f, l->u.z);
		default:
			goto non_numeric;
		}
		break;
	case V_FP:
		switch (r->type) {
		case V_INT:
			return mpfr_cmp_z(l->u.f, r->u.z);
		case V_FP:
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


@*1 Operations.
We can perform operations on values.
The tricky part is that values may have different types, and we
must handle all possible cases.

@ Function |val_un| performs a type-generic unary operation.
It returns a pointer to a |val| object, reusing the input
argument if possible (i.e., if that object is floating).

For floating point operations, the result precision is the same as the
input precision.
(This is probably a bug: it's inconsistent with binary operations.)

@c
struct val *val_un(struct val *v,
	@|void @[(*fz)@](mpz_t, const mpz_t),
	@|int @[(*ff)@](mpfr_t, const mpfr_t, mpfr_rnd_t))
{
	struct val *u = 0;

	assert(v != NULL);
	switch (v->type) {
	case V_INT:
		if (fz != NULL) {
			u = v->refcnt ? new_int() : v;
			(*fz)(u->u.z, v->u.z);
			break;
		}
		v = val_to_fp(v);
		/* fall through */
	case V_FP:
		if (ff != NULL) {
			u = v->refcnt ? new_fp(mpfr_get_prec(v->u.f)) : v;
			(*ff)(u->u.f, v->u.f, _rnd_mode);
		} else
			complain("operation not supported\n");
		break;
	default:
		complain("non-numeric value\n");
		break;
	}
	return u;
}


@ Function |val_bin| performs a type-generic binary operation.

Handling mixed-type arguments is tricky:
we can make use of the mixed-type functions in GMP and MPFR libraries
(e.g., |mpfr_add_z|) when possible;
otherwise, convert both to floating point and then perform the calculation.

@c
struct val *val_bin(struct val *l, struct val *r,
	@|void @[(*fzz)@](mpz_t, const mpz_t, const mpz_t),
	@|int @[(*fzf)@](mpfr_t, const mpfr_t, const mpz_t, mpfr_rnd_t),
	@|int @[(*ffz)@](mpfr_t, const mpfr_t, const mpz_t, mpfr_rnd_t),
	@|int @[(*fff)@](mpfr_t, const mpfr_t, const mpfr_t, mpfr_rnd_t))
{
	struct val *u = NULL;

	assert(l != NULL && r != NULL);
reswitch:
	switch (l->type) {
	case V_INT:
		switch (r->type) {
		case V_INT:
			@<Binary operations on integers@>;
			break;
		case V_FP:
			@<Binary operations on integer and floating point@>;
			break;
		default:
			goto non_numeric;
		}

		break;
	case V_FP:
		switch (r->type) {
		case V_INT:
			@<Binary operations on floating point and integer@>;
			break;
		case V_FP:
			@<Binary operations on floating points@>;
			break;
		default:
			goto non_numeric;
		}
		break;
	default:
non_numeric:
		complain("non_numeric value\n");
		return NULL;
	}
	return u;
not_supported:
	complain("operation not supported\n");
	return NULL;
}

@ When two operands are integer, call {\it mpz\_*} functions.

We reuse the operand for the result if possible.

@<Binary operations on integers@>=
if (fzz == NULL)
	goto not_supported;
u = (l->refcnt == 0) ? l : (r->refcnt == 0) ? r : new_int();
(*fzz)(u->u.z, l->u.z, r->u.z);

@ When |l| is integer and |r| is floating point
and a symmetric function is available,
swap |l| and |r| and reduce to the symmetric case.

Otherwise, convert |l| to floating point and retry;
Because the conversion may create a new floating object,
we must use it for the output.

@<Binary operations on integer and floating point@>=
{
	if (fzf == NULL)
		u = l = val_to_fp(l);
	else {
		struct val *tmp;

		tmp = l;
		l = r;
		r = tmp;
		ffz = fzf;
	}
	goto reswitch;
}

@ When |l| is floating point and |r| is integer, try
{\it mpfr\_*\_z} functions.
If no function is available, convert |r| to floating point and retry.

We can reuse |l| for the result as long as the its precision matches
the current precision.  Otherwise, we create a new floating point value.

@d CAN_REUSE_FP(v) ((v)->refcnt == 0 && mpfr_get_prec((v)->u.f) == _precision)
@<Binary operations on floating point and integer@>=
if (ffz == NULL) {
	u = r = val_to_fp(r);
	goto reswitch;
}
assert(u == NULL);
u = CAN_REUSE_FP(l) ? l : new_fp(_precision);
(*ffz)(u->u.f, l->u.f, r->u.z, _rnd_mode);

@ When two operands are floating point, call {\it mpfr\_*} functions.

@<Binary operations on floating points@>=
if (fff == NULL)
	goto not_supported;
if (u == NULL)
	u = CAN_REUSE_FP(l) ? l : CAN_REUSE_FP(r) ? r : new_fp(_precision);
(*fff)(u->u.f, l->u.f, r->u.f, _rnd_mode);


@* Arrays.  Many \.{dc} implementations support array operations.
Arrays are indexed by integers.  The tricky thing is that the dimension of
an array is not predefined, any the user may use any integer as the index.
This program supports any index within the range $[0,\.{ULONG\_MAX}]$.

We can't preallocate space for arrays; there are too many possible indices.
Instead, this program will use a binary search tree to store all the indices
and values.

@c
struct bst {
	unsigned long k;
	struct val *v;
	struct bst *l, *r;
} *root;

@ Binary search trees has a potential performance issue: imbalance.
To mitigate that, we implement the top-down splaying algorithm that
guarantees $\Theta(\log n)$ amortized time for the operations.

For details, see Sleator and Tarjan's paper
``Self-adjusting binary search trees'' (1985).
@c
struct bst *bst_splay(struct bst *n, unsigned long k)
{
	struct bst dummy;
	struct bst *l, *r, *m;

	l = r = &dummy;
	for (;;)
		if (k < n->k && (m = n->l) != NULL) {
			if (k < m->k && m->l != NULL)
				@<Rotate right@>;
			@<Link right@>;
		} else if (k > n->k && (m = n->r) != NULL) {
			if (k > m->k && m->r != NULL)
				@<Rotate left@>;
			@<Link left@>;
		} else
			break;
	l->r = n->l;
	r->l = n->r;
	n->l = dummy.r;
	n->r = dummy.l;
	return n;
}

@ BST left rotation (infix notation $(xAy)$ denotes a subtree
with root~$A$, left child~$x$ and right child~$y$).

$$(xA(yBz))\;\longrightarrow\;((xAy)Bz)$$
@<Rotate left@>=
{
	n->r = m->l;
	m->l = n;
	n = m;
}

@ BST right rotation
$$((xAy)Bz)\;\longrightarrow\;(xA(yBz))$$
@<Rotate right@>=
{
	n->l = m->r;
	m->r = n;
	n = m;
}

@ @<Link left@>=
l = l->r = n;
n = n->r;

@ @<Link right@>=
r = r->l = n;
n = n->l;

@ Function |bst_get| returns a value given an index.
@c
struct bst *bst_get(struct bst *n, unsigned long k, struct val **ptr)
{
	if (n == NULL || (n = bst_splay(n, k), n->k != k))
		*ptr = NULL;
	else
		*ptr = n->v;
	return n;
}

@ Function |bst_set| sets the value (and increments its reference count)
at the given index.
@c
struct bst *bst_set(struct bst *n, unsigned long k, struct val *v)
{
	struct bst *m;

	if (n != NULL && (n = bst_splay(n, k), n->k == k)) {
		val_unref(n->v);
		if (v == NULL) {
			if (n->l == NULL)
				m = n->r;
			else if (n->r == NULL)
				m = n->l;
			else {
				m = n->r = bst_splay(n->r, 0);
				assert(m->l == NULL);
				m->l = n->l;
			}
			free(n);
			return m;
		}
		n->v = val_ref(v);
		return n;
	} else if (v != NULL) {
		m = malloc(sizeof *m);
		checkptr(m);
		m->k = k;
		m->v = val_ref(v);
		if (n == NULL)
			m->l = m->r = NULL;
		else if (k < n->k) {
			m->l = n->l;
			n->l = NULL;
			m->r = n;
		} else if (k > n->k) {
			m->l = n;
			m->r = n->r;
			n->r = NULL;
		}
		return m;
	}
	return n;
}

@ Function |bst_clr| releases the entire tree.
@c
void bst_clr(struct bst *n)
{
	struct bst *m;

	while (n) {
		if (n->l == NULL)
			m = n->r;
		else if (n->r == NULL)
			m = n->l;
		else {
			n = bst_splay(n, 0);
			m = n->r;
		}
		val_unref(n->v);
		free(n);
		n = m;
	}
}


@* Stacks and registers.
This program operates on a stack and several ``registers''.
A register is really just a stack but with a name associated to it.
We will use the |stk| structure to represent a stack.

@s stk int
@d stk_top(s, i) ((s)->v[(s)->size-(i)])
@c
struct stk {
	struct val **v; /* values */
	struct bst **a; /* associated arrays */
	size_t size, cap;
};

@ Function |stk_res| resizes the capacity of the stack.
It is used by |stk_push| and |stk_pop| to dynamically adjust the memory
the stack uses.
@c
void stk_res(struct stk *s, size_t cap)
{
	if (cap == 0)
		cap = 1;
	assert(cap >= s->size);
	s->v = realloc(s->v, sizeof (*s->v) * cap);
	checkptr(s->v);
	s->a = realloc(s->a, sizeof (*s->a) * cap);
	checkptr(s->a);
	s->cap = cap;
}

@ Function |stk_push| pushes a value onto the stack and increments
its reference count.

@c
void stk_push(struct stk *s, struct val *v)
{
	assert(v != NULL);
	if (s->size == s->cap)
		stk_res(s, s->cap * 2);
	s->v[s->size] = val_ref(v);
	s->a[s->size] = NULL; /* a new array level is introduced */
	++s->size;
}

@ Function |stk_pop| pops a value from the stack and decrements
its reference count.  The popped value may be floating.

Note, in a rare case, the top of the stack may be NULL: when the array part
is initialized but no value has been saved into the register.
It won't happen with the main stack: the main stack does not have arrays
stored in it, but we must be extremely careful when dealing with registers.

We also want to shrink the memory when appropriate.
The rule used here is once the size of the stack is less than $1/4$ of its
capacity, the stack is shrinked to half of its capacity
(and will become half-full).
However, to avoid frequent reallocations, we only shrink if the capacity
is greater than a threshold.

@c
struct val *stk_pop(struct stk *s)
{
	assert (s->size > 0);
	--s->size;
	if (s->v[s->size] != NULL)
		val_deref(s->v[s->size]);
	bst_clr(s->a[s->size]);
	if (s->cap > 64 && s->size < s->cap / 4)
		stk_res(s, s->cap / 2);
	return s->v[s->size];
}


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
If the input source is a string, then the |s|~field is non-NULL,
and the |v|~field in the union references the value object.

(Note: \POSIX/ offers |fmemopen| for creating a |FILE*| given a string;
it's not used in this program.)

Lastly, the token scanner is actually stacked.  Initially, the program
is reading from a file. When it starts to execute a macro, a new token
scanner is pushed onto the stack, and is popped when the macro finishes.
The |link| field indicates the previous token scanner in the stack.

@d LEX_ISFILE(l) ((l)->s == NULL)
@s lex int
@c
struct lex {
	union {
		FILE *f;
		struct val *v;
	} u;
	const char *s;
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
	l->s = NULL;
	l->link = top;
	return l;
}

@ Function |lex_push_str| pushes a token scanner that reads from a string.

If the token scanner has reached the end, we can discard the top before
pushing the new token scanner.  This enables efficient tail recursion.

@c
struct lex *lex_push_str(struct lex *top, struct val *v)
{
	struct lex *l;

	assert(v->type == V_STR);
	if (top && !LEX_ISFILE(top) && !*top->s) {
		l = top;
		val_unref(top->u.v);
	} else {
		l = malloc(sizeof *l);
		checkptr(l);
		l->link = top;
	}
	l->u.v = val_ref(v);
	l->s = v->u.s.ptr;
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
		val_unref(top->u.v);
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
	else if ((ch = *l->s & UCHAR_MAX) != '\0')
		++l->s; /* don't read pass |'\0'| */
	return ch ? ch : EOF;
}

@ Function |lex_ungetc| pushes back a character to the input.
This is necessary because the token scanner sometimes reads ahead.
Basically, one should only ``unget'' a single character that has just
been read.

@c
static int lex_ungetc(int ch, struct lex *l)
{
	if (ch == EOF)
		return ch;
	if (LEX_ISFILE(l))
		return ungetc(ch, l->u.f);
	return *--l->s;
}

@ Function |lex_gettok| reads a token from the input.
\item{$\bullet$} For a character token, the return value is its character code.
\item{$\bullet$} For a value token, it returns zero, and the value is pushed
onto the main stack.
\item{$\bullet$} In case of end-of-file or end-of-string, it returns |EOF|.

The rules for a value token loosely follows the \.{dc} convention:
the characters that start a value token are \.{[0-9A-F\_.[]}.

@c
struct val *read_str(struct lex *);
struct val *read_num(struct lex *);
int lex_gettok(struct lex *l, struct val **v)
{
	int ch;

	switch (ch = lex_getc(l)) {
	case '0': case '1': case '2': case '3': case '4': @|
	case '5': case '6': case '7': case '8': case '9': @|
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': @|
	case '_': case '.':
		lex_ungetc(ch, l);
		*v = read_num(l);
		return 0;
	case '[':
		*v = read_str(l);
		return 0;
	}
	return ch;
}

@* Reading values from input.
Other than reading single-character tokens, this section deals with
reading a numeric or string value from the input.

One of the difficulties is that the input size is unbounded.
The solution is to use a dynamic buffer that grows if the input exceeds
its capacity.

To make the common case fast, we start with a pre-allocated short buffer.
For short input, we can save time from heap allocations.

@d SHORTBUF_SIZE 256
@c
struct buf {
	char *ptr;
	size_t len, cap;
	char shortbuf[SHORTBUF_SIZE];
};

void buf_ini(struct buf *b)
{
	b->ptr = b->shortbuf;
	b->len = 0;
	b->cap = SHORTBUF_SIZE;
}

void buf_free(struct buf *b)
{
	if (b->ptr != b->shortbuf)
		free(b->ptr);
}

@ Function |buf_res| resizes the buffer.
It is similar to |stk_res| but also handle the additional complexity
associated to the short buffer.

@c
void buf_res(struct buf *b, size_t cap)
{
	if (b->ptr == b->shortbuf) {
		b->ptr = malloc(cap);
		checkptr(b->ptr);
		memcpy(b->ptr, b->shortbuf, b->len);
	} else {
		b->ptr = realloc(b->ptr, cap);
		checkptr(b->ptr);
	}
	b->cap = cap;
}

@ Function |buf_add| adds a character to the buffer.
@c
void buf_add(struct buf *b, int ch)
{
	if (b->len == b->cap)
		buf_res(b, b->cap * 2);
	b->ptr[b->len++] = ch;
}

@ Function |buf_fin| terminates the buffer by appending
a |'\0'| character to its end.

If using heap memory, shrink it so that no extra memory is allocated.

@c
void buf_fin(struct buf *b)
{
	if (b->ptr != b->shortbuf || b->len == b->cap)
		buf_res(b, b->len + 1);
	b->ptr[b->len] = '\0';
}

@ A string token is a character sequenced enclosed by a balanced pair of
brackets.  It's unfortunate that there is no escaping so one cannot have
a string containing unbalanced brackets. (Bad for the user, good for the
implementer, though.)

@c
struct val *read_str(struct lex *l)
{
	struct buf B;
	int balance = 1;
	int ch;

	buf_ini(&B);

	for (;;) {
		switch (ch = lex_getc(l)) {
		case '[': ++balance; break;
		case ']': --balance; break;
		case EOF: balance = 0;
			complain("unbalanced []\n");
			break;
		}
		if (balance == 0)
			break;
		buf_add(&B, ch);
	}
	buf_fin(&B);
	if (B.ptr == B.shortbuf)
		B.ptr = memdup(B.ptr, B.len+1);
	return new_str(B.ptr, B.len);
}

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

@c
struct val *read_num(struct lex *l)
{
	struct buf B;
	int isint = 1;
	int ch;

	buf_ini(&B);
	ch = lex_getc(l);
	@<Read an optional negative sign@>;
	@<Scan hexdigit sequence@>;
	@<Read an optional fractional part@>;
	@<Read an optional exponent@>;
	lex_ungetc(ch, l); /* push back the read-ahead */
	@<Return a numeric value@>;
}

@ We use \.{\_} for negative sign, but GMP and MPFR use \.{-}.
@<Read an optional negative sign@>=
if (ch == '_') {
	ch = '-';
	buf_add(&B, ch);
	ch = lex_getc(l);
}

@ If we see a decimal point, a fractional part follows.
@<Read an optional fractional part@>=
if (ch == '.') {
	isint = 0;
	buf_add(&B, ch);
	ch = lex_getc(l);
	@<Scan hexdigit sequence@>;
}

@ If we see an \.{e}, an exponent follows.
@<Read an optional exponent@>=
if (ch == 'e') {
	isint = 0;
	ch = '@@'; /* don't confuse MPFR when |_ibase>10| */
	buf_add(&B, ch);
	ch = lex_getc(l);
	if (ch == '+' || ch == '-') {
		buf_add(&B, ch);
		ch = lex_getc(l);
	}
	@<Scan hexdigit sequence@>;
}

@ @<Scan hexdigit sequence@>=
while (isdigit(ch) || (isxdigit(ch) && isupper(ch))) {
	buf_add(&B, ch);
	ch = lex_getc(l);
}

@ @<Return a numeric value@>=
{
	struct val *v;
	int rc;

	buf_fin(&B);
	if (isint) {
		v = new_int();
		rc = mpz_set_str(v->u.z, B.ptr, (B.len == 1) ? 16 : _ibase);
	} else {
		v = new_fp(_precision);
		rc = mpfr_set_str(v->u.f, B.ptr, _ibase, _rnd_mode);
	}
	if (rc == -1)
		complain("malformed number %s in base %d\n", B.ptr, _ibase);
	buf_free(&B);
	return v;
}

@* Writing values to output.
Function |val_put| writes the value to a file.
It comes with two formats: the normal format and the special format,
determined by the |special| argument.

@c
void val_put(struct val *v, FILE *f, int special)
{
	switch (v->type) {
	case V_INT:
		@<Write an integer value@>;
		break;
	case V_FP:
		@<Write a floating point value@>;
		break;
	case V_STR:
		fputs(v->u.s.ptr, f);
		break;
	}
}

@ Writing an integer is easy: |mpz_out_str| does the job.
Function |mpz_out_str| will use uppercase letters if base is negative.

@<Write an integer value@>=
if (!special)
	mpz_out_str(f, -_obase, v->u.z);
else
	@<Write an integer in special format@>;

@ When writing an integer in the special format,
Intepret the number as a byte stream, e.g.,
for the number 15711167 (or |0xefbbbf|), we will write three bytes:
|0xef|, |0xbb| and |0xbf|.

@<Write an integer in special format@>=
{
	size_t n, i;
	const mp_limb_t *limbs;

	if ((n = mpz_size(v->u.z)) == 0)
		putchar('\0');
	else {
		limbs = mpz_limbs_read(v->u.z);

#define BYTE (limbs[n-1] >> ((i-1)*CHAR_BIT))
		for (i = sizeof (mp_limb_t); i && BYTE == 0; i--)
			;
		for (; i; i--)
			putc(BYTE & UCHAR_MAX, f);
		while (--n)
			for (i = sizeof (mp_limb_t); i; i--)
				putc(BYTE & UCHAR_MAX, f);
#undef BYTE
	}
}

@ Writing a floating point number is tricky.

First, there are special values (0, $\pm\infty$ or NaN).
For these values, we call |mpfr_out_str| directly for special values.
They will show up as \.{@@Inf@@}, \.{-@@Inf@@} or \.{@@NaN@@}.

@<Write a floating point...@>=
if (!mpfr_number_p(v->u.f))
	mpfr_out_str(f, _obase, 0, v->u.f, _rnd_mode);
else
	@<Write a normal floating point value@>;

@ When writing a normal number, we call |mpfr_get_str| as it gives more
flexibility: we can truncate trailing zeros and/or
omit the exponent if we desire.

However, MPFR does not support a negative base and uses lowercase letters
by default, so we need to convert them to uppercase manually by calling
|toupper|.

@<Write a normal...@>=
{
	size_t n, len, i = 0;
	char *s;
	mpfr_exp_t e;

	@<Determine output digits |n|@>;
	s = mpfr_get_str(NULL, &e, _obase, n, v->u.f, _rnd_mode);
	len = strlen(s);
	@<If negative, output and skip the minus sign@>;
	@<Write digits before decimal point@>;
	putc('.', f); /* writes the decimal point */
	@<Write digits after decimal point@>;
	@<Write the exponent if necessary@>;
	mpfr_free_str(s);
}

@ The second difficulty is to determine how many digits to output.
For a $k$-bit floating point, we output no more than
$\lfloor k \log_b 2 \rfloor$ digits in base~$b$.
The printed result is accurate if $k=2$.

If writing in the special format, we will set $n=0$ and let MPFR determine
how many digits to write.
The printed result is always accurate (for the purpose of reading back).

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
		n = mpfr_get_prec(v->u.f);
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

@
Finally, we need to write the decimal point and the exponent properlly.
We will omit the exponent if |e| is between 0 and |len|.
In this case, we need to place the decimal point at a proper place,
i.e., after the |e|-th digit.

Note: zero is really a special case because |e==0| for a zero value,
but we do want one digit before the decimal point.

@<Write digits before...@>=
if (mpfr_zero_p(v->u.f))
	e = 1;
if (!special && 0 <= e && (size_t) e <= len)
	for (; e; e--)
		putc(toupper(s[i++]), f);
else
	putc(toupper(s[i++]), f); /* only 1 digit before the decimal point */

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
	if (special || e)
		fprintf(f, "e%ld", (long) (e-1));


@* Interpreter.  The input is interpreted as tokens are read.
The interpreter can manipulate the stack, the registers, and the token scanner.

Function |interpret| reads tokens from the token scanner~|l|, executes them
until no more tokens are available.

The argument~|r| is an array of registers.  There are |UCHAR_MAX| of them,
numbered from 1 to |UCHAR_MAX|.  |r[0]| is the main stack and cannot be
used as a normal register.

@c
@<Auxiliary functions for the interpreter@>@;
void interpret(struct lex *l, struct stk r[])
{
	struct val *x, *y, *z, *v = NULL;
	struct stk *rr;
	int tok;

	while (l != NULL) {
		tok = lex_gettok(l, &v);
		switch (tok) {
		@<Actions for input token@>;
		@<Interpreter error conditions@>;
		}
	}
}

@ When the input token is a space character, no action is required.
@<Actions...@>+=
case ' ': case '\t': case '\v': case '\f':
case '\r':
	break; /* no action */

@ When a valid is read (|tok == '\0'|), push it onto the stack.
@<Actions...@>+=
case '\0':
	stk_push(r, v);
	break;

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
	while (r->size)
		val_ckref(stk_pop(r));
	break;

@ The \.{d} command duplicates the stack top.
@d CHK(n) if (r->size < n) goto stack_empty
@<Actions...@>+=
case 'd':
	CHK(1);
	stk_push(r, stk_top(r, 1));
	break;

@ The \.{f} command dumps the entire stack (from top to bottom).
@<Actions...@>+=
case 'f':
{
	size_t i;

	for (i = r->size; i > 0; ) {
		val_put(r->v[--i], stdout, 0);
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
	CHK(1);
	val_put(stk_top(r, 1), stdout, tok == 'P');
	if (tok == 'p')
		fputc('\n', stdout);
	else
		val_ckref(stk_pop(r));
	break;

@ The \.{r} command swaps the top of the stack and the second of the
stack.
@<Actions...@>+=
case 'r':
	CHK(2);
	x = stk_top(r, 1);
	y = stk_top(r, 2);
	stk_top(r, 1) = y;
	stk_top(r, 2) = x;
	break;

@ The \.{z} command pushes the depth of the stack onto the stack.
@<Actions...@>+=
case 'z':
	v = new_int();
	mpz_set_ui(v->u.z, r->size);
	stk_push(r, v);
	break;

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
	unsigned long n;

	CHK(1);
	x = stk_pop(r);
	val_to_ui(x, &n);
	val_ckref(x);
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
	CHK(1);
	x = stk_pop(r);
	if (x->type == V_STR)
		l = lex_push_str(l, x);
	else
		stk_push(r, x);
	break;

@ The \.{!} command reads a line and invokes an external command
as indicated by the line.

@<Actions...@>+=
case '!':
{
	struct buf B;
	int ch;

	buf_ini(&B);
	while ((ch = lex_getc(l)) != EOF && ch != '\n')
		buf_add(&B, ch);
	buf_fin(&B);
	if (system(B.ptr) == -1)
		perror(progname);
	buf_free(&B);
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
	int rc;

	mpfr_clear_erangeflag();
	if (r->size < 2) {
		complain("stack empty\n");
		rc = 0;
	} else {
		x = stk_pop(r);
		y = stk_pop(r);
		rc = val_cmp(x, y);
		val_ckref2(x, y);
	}
	if ((rr = get_reg(l, r, 1)) == NULL)
		break;

	if (!mpfr_erangeflag_p() &&
			((tok == '<' && rc < 0) ||
			(tok == '=' && rc == 0) ||
			(tok == '>' && rc > 0))) {
		z = stk_top(rr, 1);
		assert(rr->size > 0 && z != NULL);
		if (z->type == V_STR)
			l = lex_push_str(l, z);
		else
			stk_push(r, z);
	}
	break;
}

@ Function |get_reg| reads a character from the input and use its
character code to determine the register.

@<Auxiliary functions for the interpreter@>+=
struct stk *get_reg(struct lex *l, struct stk r[], int non_empty)
{
	struct stk *rr;
	int i;

	if ((i = lex_getc(l)) == EOF) {
		complain("unexpected EO%c\n", LEX_ISFILE(l) ? 'F' : 'S');
		return NULL;
	}
	assert(i != 0);
	rr = &r[i];
	if (non_empty && (rr->size == 0 || stk_top(rr, 1) == NULL)) {
		complain("register %s is empty\n", tok2str(i));
		return NULL;
	}
	return rr;
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
	if ((rr = get_reg(l, r, 0)) != NULL) {
		CHK(1);
		x = stk_pop(r);
		if (tok == 'S' || rr->size == 0)
			stk_push(rr, x);
		else {
			if (stk_top(rr, 1) != NULL)
				val_unref(stk_top(rr, 1));
			stk_top(rr, 1) = val_ref(x);
		}
	}
	break;

@ The \.{l}~command loads the value in a register and pushes it onto
the stack.  The value of the register keeps unchanged.

The \.{L}~command loads the value in a register and pushes it onto
the stack.  The register is restored to its previous value.

@<Actions...@>+=
case 'l':
case 'L':
	if ((rr = get_reg(l, r, 1)) != NULL)
		stk_push(r, (tok == 'l') ? stk_top(rr, 1) : stk_pop(rr));
	break;

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
case '+': case '-': case '*': case '/': case '%': case '^':
	CHK(2);
	y = stk_pop(r);
	x = stk_pop(r);
	z = NULL;
	switch (tok) {
	case '+':
		z = val_bin(x, y, mpz_add, mpfr_add_z, mpfr_add_z, mpfr_add);
		break;
	case '-':
		z = val_bin(x, y, mpz_sub, my_mpfr_z_sub, mpfr_sub_z, mpfr_sub);
		break;
	case '*':
		z = val_bin(x, y, mpz_mul, mpfr_mul_z, mpfr_mul_z, mpfr_mul);
		break;
	case '^':
		z = val_bin(x, y, my_mpz_pow, NULL, mpfr_pow_z, mpfr_pow);
		break;
	case '/':
		z = val_bin(x, y, my_mpz_tdiv_q, NULL, mpfr_div_z, mpfr_div);
		break;
	case '%':
		z = val_bin(x, y, my_mpz_tdiv_r, NULL, NULL, mpfr_fmod);
		break;
	}
	if (z != NULL)
		stk_push(r, z);
	val_ckref2(x, y);
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

@ Function |my_mpz_tdiv_q| and |my_mpz_tdiv_r| complains in case of division
by zero, instead of signaling floating point exception.
@<Auxiliary functions for the interpreter@>+=
void my_mpz_tdiv_q(mpz_t z, const mpz_t x, const mpz_t y)
{
	if (mpz_cmp_ui(y, 0) == 0)
		complain("division by zero\n");
	else
		mpz_tdiv_q(z, x, y);
}

void my_mpz_tdiv_r(mpz_t z, const mpz_t x, const mpz_t y)
{
	if (mpz_cmp_ui(y, 0) == 0)
		complain("division by zero\n");
	else
		mpz_tdiv_r(z, x, y);
}

@*1 Integer-specific operations.
Some operations are only valid on integer operands:
\item{$\bullet$} Quotient and remainder;
\item{$\bullet$} Power modulo;
\item{$\bullet$} Bitwise operations.

@ Command \.{\~} takes two integers from the stack and pushes
the quotient and remainder back.

@<Actions...@>+=
case '~':
	CHK(2);

	y = stk_pop(r);
	x = stk_pop(r);
	z = (x->refcnt == 0) ? x : new_int();
	v = (x != y && y->refcnt == 0) ? y : new_int();
	if (x->type != V_INT && y->type != V_INT)
		complain("non-integer value\n");
	else
		mpz_tdiv_qr(z->u.z, v->u.z, x->u.z, y->u.z);
	stk_push(r, z);
	stk_push(r, v);
	val_ckref2(x, y);
	break;

@ Command \.{\char"7C} is actually a ternary operator.
It takes three integers $x$, $y$, $m$ and returns $x^y \bmod m$
(borrowed from GNU \.{dc}).
It can be useful if $x^y$ is very large.

@<Actions...@>+=
case '|':
	CHK(3);

	z = stk_pop(r);
	y = stk_pop(r);
	x = stk_pop(r);
	v = (x->refcnt == 0) ? x : new_int();
	if (x->type != V_INT || y->type != V_INT || z->type != V_INT)
		complain("non-integer value\n");
	else if (mpz_cmp_ui(y->u.z, 0) == 0 || mpz_cmp_ui(z->u.z, 0) == 0)
		complain("division by zero\n");
	else
		mpz_powm(v->u.z, x->u.z, y->u.z, z->u.z);
	stk_push(r, v);
	val_ckref3(x, y, z);
	break;

@ Bitwise operators are prefixed by \.{\&} and are summarized below.
Negative numbers are treated as two's complement with an infinite
number of leading ones.
$$\tabskip=1em\vbox{\halign{
\strut\tt\hfil#\hfil&\hfil$#$\hfil&#\hfil\cr
\noalign{\hrule\kern1pt}
\rm Operator&\hbox{Result}&Notes\cr
\noalign{\hrule\kern1pt}
\char"26& x\wedge y&Bitwise and\cr
\char"7C& x\vee y&Bitwise (inclusive) or\cr
\^{}& x\oplus y&Bitwise exclusive or (XOR)\cr
\~{}& \lnot x&One's complement\cr
\#& \hbox{\# of 1's in }x&Population count; $-1$ if $x$ is negative\cr
<& x\ll y&Left shift; $y$ must be non-negative, same below\cr
>& x\gg y&Right shift\cr
s& x\vee (1\ll y)&Set bit\cr
c& x\wedge \lnot (1\ll y)&Clear bit\cr
t& x\oplus (1\ll y)&Toggle bit\cr
T& \cases{1&if $x\wedge (1\ll y)\ne0$\cr\mathstrut0&otherwise}&
Test bit\cr
\noalign{\hrule}
}}$$
All bitwise operations are extensions.

@<Actions...@>+=
case '&':
	switch (tok = lex_getc(l)) {
	default:
		complain("bitwise operator %s is not implemented\n",
			tok2str(tok));
	case EOF: break;
	@<Binary bitwise operators@>@;
	@<Unary bitwise operators@>@;
	}
	break;

@ @<Binary bitwise operators@>=
case '&': case '|': case '^':
	CHK(2);
	y = stk_pop(r);
	x = stk_pop(r);
	z = NULL;
	switch (tok) {
	case '&':
		z = val_bin(x, y, mpz_and, NULL, NULL, NULL);
		break;
	case '|':
		z = val_bin(x, y, mpz_ior, NULL, NULL, NULL);
		break;
	case '^':
		z = val_bin(x, y, mpz_xor, NULL, NULL, NULL);
		break;
	}
	if (z != NULL)
		stk_push(r, z);
	val_ckref2(x, y);
	break;

@ @<Binary bitwise operators@>=
case '<': case '>':
case 's': case 'c': case 't': case 'T':
{
	unsigned long n;
	int ok;

	CHK(2);
	x = stk_pop(r);
	ok = val_to_ui(x, &n);
	val_ckref(x);
	if (!ok) {
		complain("infeasible shift amount\n");
		break;
	}
	x = stk_pop(r);
	if (x->type != V_INT)
		complain("non-integer value\n");
	else {
		y = (x->refcnt == 0) ? x : new_int();
		switch (tok) {
		case '<':
			mpz_mul_2exp(y->u.z, x->u.z, n);
			break;
		case '>':
			mpz_div_2exp(y->u.z, x->u.z, n);
			break;
		case 's':
			if (x != y)
				mpz_set(y->u.z, x->u.z);
			mpz_setbit(y->u.z, n);
			break;
		case 'c':
			if (x != y)
				mpz_set(y->u.z, x->u.z);
			mpz_clrbit(y->u.z, n);
			break;
		case 't':
			if (x != y)
				mpz_set(y->u.z, x->u.z);
			mpz_combit(y->u.z, n);
			break;
		case 'T':
			mpz_set_ui(y->u.z, mpz_tstbit(x->u.z, n));
			break;
		}
		stk_push(r, y);
	}
	val_ckref(x);
	break;
}

@ @<Unary bitwise operators@>+=
case '~':
	CHK(1);
	x = stk_pop(r);
	y = val_un(x, mpz_com, NULL);
	if (y != NULL)
		stk_push(r, y);
	val_ckref(x);
	break;

@ @<Unary bitwise operators@>+=
case '#':
	CHK(1);
	x = stk_pop(r);
	if (x->type != V_INT)
		complain("non-integer value\n");
	else {
		y = (x->refcnt == 0) ? x : new_int();
		if (mpz_cmp_ui(x->u.z, 0) >= 0)
			mpz_set_ui(y->u.z, mpz_popcount(x->u.z));
		else
			mpz_set_si(y->u.z, -1);
		stk_push(r, y);
	}
	val_ckref(x);
	break;

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
z&  \zeta(x)&Extension; $\zeta(x)=\sum_{n=1}^\infty {1\over n^x\mathstrut}$\cr
g&  \Gamma(x)&Extension; $\Gamma(x)=\int_0^\infty t^{x-1}e^{-t}\;dt$\cr
G&  \ln\Gamma(x)&Extension\cr
r&  x\hbox{ rounded to integer}&Extension\cr
\noalign{\kern1pt\hrule}
}}$$
Note: only \.{v} is a dedicated command.  Other operations must be preceded
by \.{m}.

@<Actions...@>+=
case 'v':
	CHK(1);
	x = stk_pop(r);
	y = val_un(x, my_mpz_sqrt, mpfr_sqrt);
	if (y)
		stk_push(r, y);
	val_ckref(x);
	break;
case 'm':
	tok = lex_getc(l);
	CHK(1);
	x = stk_pop(r);
	y = NULL;
	switch (tok) {
	case EOF: break; /* invalid */
	@<Math library functions@>;
	default:
		complain("math function %s not implmented\n", tok2str(tok));
		break;
	}
	if (y)
		stk_push(r, y);
	val_ckref(x);
	break;

@ @<Auxiliary functions for the interpreter@>+=
void my_mpz_sqrt(mpz_t z, const mpz_t x)
{
	if (mpz_cmp_ui(x, 0) < 0)
		complain("square root of negative number\n");
	else
		mpz_sqrt(z, x);
}

@ For unary math functions, call |reg_un|.
@<Math library functions@>=
case 's': y = val_un(x, NULL, mpfr_sin); break;
case 'c': y = val_un(x, NULL, mpfr_cos); break;
case 'a': y = val_un(x, NULL, mpfr_atan); break;
case 'l': y = val_un(x, NULL, mpfr_log); break;
case 'e': y = val_un(x, NULL, mpfr_exp); break;
case 'z': y = val_un(x, NULL, mpfr_zeta); break;
case 'g': y = val_un(x, NULL, mpfr_gamma); break;
case 'G': y = val_un(x, NULL, mpfr_lngamma); break;
case 'r': y = val_un(x, NULL, mpfr_rint); break;

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
	long n;

	val_to_si(x, &n);
	val_ckref(x);
	CHK(1);
	x = stk_pop(r);
	if ((z = val_to_fp(x)) != NULL) {
		y = CAN_REUSE_FP(z) ? z : new_fp(_precision);
		((tok == 'j') ? mpfr_jn : mpfr_yn)
			(y->u.f, n, z->u.f, _rnd_mode);
	}
	break;
}

@*1 Array operations.

The \.{:} and \.{;} commands operate on arrays, using the top
of the stack as the index~$i$.
Command \.{:}$x$ saves the second of the stack to $x[i]$;
command \.{;}$x$ loads $x[i]$ to the stack.

@<Actions...@>+=
case ':':
case ';':
{
	unsigned long k = 0;
	int ok;
	struct bst **root;

	if ((rr = get_reg(l, r, 0)) == NULL)
		break;

	CHK(1);
	y = stk_pop(r);
	ok = val_to_ui(y, &k);
	if (!ok)
		complain("array index must be a non-negative integer\n");
	val_ckref(y);
	if (rr->size == 0) {
		if (rr->cap == 0)
			stk_res(rr, 1);
		rr->v[0] = NULL;
		rr->a[0] = NULL;
		++rr->size;
	}
	root = &rr->a[rr->size-1];
	if (tok == ':') {
		CHK(1);
		x = stk_pop(r);
		if (ok)
			*root = bst_set(*root, k, x);
	} else if (ok) {
		*root = bst_get(*root, k, &v);
		stk_push(r, v ? v : new_int());
	}
	break;
}

@*1 Parameters.  The parameters in the global state can be changed
by the commands in this section.

@ The \.{i} and \.{o} commands set the input and output base, respectively.
@<Actions...@>+=
case 'i':
case 'o':
{
	long b;

	CHK(1);

	x = stk_pop(r);
	val_to_si(x, &b);
	val_ckref(x);
	if (b < 2)
		complain("base too small; set to %ld\n", b=2);
	else if (b > 16)
		complain("base too large; set to %ld\n", b=16);
	if (tok == 'i')
		_ibase = b;
	else
		_obase = b;
	break;
}

@ The \.{I} and \.{O} commands push the current input and output base onto
the stack, respectively.
@<Actions...@>+=
case 'I':
case 'O':
	x = new_int();
	mpz_set_si(x->u.z, (tok == 'I') ? _ibase : _obase);
	stk_push(r, x);
	break;

@ The \.{k} command sets the floating point precision.

Note: the precision is measured in bits, not decimal places.
This is different from \.{dc}.
Furthermore, the precision does not affect integer arithmetics.

@<Actions...@>+=
case 'k':
{
	unsigned long p;

	CHK(1);
	x = stk_pop(r);
	val_to_ui(x, &p);
	val_ckref(x);
	if (p < MPFR_PREC_MIN)
		complain("precision too small; set to %lu\n",
			p = MPFR_PREC_MIN);
	else if (p > MPFR_PREC_MAX)
		complain("precision too large; set to %lu\n",
			p = MPFR_PREC_MAX);
	_precision = p;
	break;
}

@ The \.{K} command pushes the current floating point precision onto the stack.
@<Actions...@>+=
case 'K':
	x = new_int();
	mpz_set_ui(x->u.z, _precision);
	stk_push(r, x);
	break;

@ The \.{u} command sets the floating point rounding mode.
@<Actions...@>+=
case 'u':
{
	long mode;

	CHK(1);
	x = stk_pop(r);
	val_to_si(x, &mode);
	val_ckref(x);
	switch (mode) {
	case 0: _rnd_mode = MPFR_RNDN; break;
	case 1: _rnd_mode = MPFR_RNDZ; break;
	case 2: _rnd_mode = MPFR_RNDU; break;
	case 3: _rnd_mode = MPFR_RNDD; break;
	case 4: _rnd_mode = MPFR_RNDA; break;
	default: complain("invalid rounding mode\n"); break;
	}
	break;
}

@ The \.{U} command pushes the current floating point precision onto the stack.
@<Actions...@>+=
case 'U':
	x = new_int();
	mpz_set_ui(x->u.z, _rnd_mode);
	stk_push(r, x);
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
	static struct stk r[UCHAR_MAX+1];
	int rc = 0;
	int read_stdin = 1;

	@<Process command line arguments@>;

	if (*argv != NULL)
		do
			exec_file(fopen(*argv++, "r"), r);
		while (*argv != NULL);
	else if (read_stdin)
		exec_file(stdin, r);

	@<Clean up registers and arrays@>;
	return rc;
}

@ Function |exec_file| executes a file.
@<Auxiliary functions for |main|@>=
void exec_file(FILE *f, struct stk r[])
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
	if (*arg == '\0' && (arg = *argv++) == NULL)
		complain("missing argument for %s\n", (--argv)[-1]);
	else if (ch == 'f')
		exec_file(fopen(arg, "r"), r);
	else {
		read_stdin = 0;
		len = strlen(arg);
		interpret(lex_push_str(NULL,
			new_str(memdup(arg, len+1), len)), r);
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

@ @<Clean up registers...@>=
{
	int i;
	struct val *v;

	for (i = 0; i <= UCHAR_MAX; i++) {
		while (r[i].size > 0)
			if ((v = stk_pop(&r[i])) != NULL)
				val_ckref(v);
		free(r[i].v);
		free(r[i].a);
	}
	mpfr_free_cache();
}

@* Index.
