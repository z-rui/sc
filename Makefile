CC=gcc
CFLAGS=-Wall -ggdb
LDFLAGS=

LIBS=-lgmp -lmpfr -lm

# Optimization level
CFLAGS+=-Os
#CFLAGS+=-DNDEBUG
CFLAGS+=-fwhole-program

# Sanitize
#CFLAGS+=-fsanitize=address -fsanitize=leak
#LDFLAGS+=-fsanitize=address -fsanitize=leak

all: sc sc.pdf

%: %.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

%.pdf: %.dvi
	dvipdfm $<

%.dvi: %.tex
	tex "\let\pdf+\input $*"

%.tex: %.w
	cweave $<

%.c: %.w
	ctangle $<

clean:
	rm -f sc.{o,c,tex,dvi,idx,scn,log,toc}

.PHONY: all clean
