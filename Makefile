CC=gcc
CFLAGS=-Wall -ggdb
LDFLAGS=

LIBS=-lgmp -lmpfr

# Optimization level
#CFLAGS+=-DNDEBUG
CFLAGS+=-O2
CFLAGS+=-fwhole-program

CFLAGS+=$(MYCFLAGS)
LDFLAGS+=$(MYLDFLAGS)

all: sc.pdf sc

%: %.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

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
.SUFFIXES:
