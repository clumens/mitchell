# $Id: Makefile,v 1.18 2004/12/21 18:18:54 chris Exp $

CC = gcc
BASE_CFLAGS = -O2 -Wall -I../include
LDFLAGS = -lgc -ldl

ifdef DEBUG
	override BASE_CFLAGS += -g
	CFLAGS = $(subst -O2,,$(BASE_CFLAGS))
else
	CFLAGS = $(BASE_CFLAGS)
endif

OBJS = absyn_printer.o list.o main.o parse.o semant.o symtab.o \
		 symtab_printer.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	-rm -f *.o mitchell
