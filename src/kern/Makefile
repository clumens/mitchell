# $Id: Makefile,v 1.19 2005/01/09 20:25:36 chris Exp $

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

# Dependancies
absyn_printer.o: absyn_printer.c \
	../include/absyn.h ../include/config.h ../include/basic_types.h \
	../include/error.h ../include/list.h

list.o: list.c \
	../include/list.h ../include/memory.h

main.o: main.c \
	../include/absyn.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/parse.h ../include/semant.h \
	../include/version.h

parse.o: parse.c \
	../include/absyn.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/tokens.h

semant.o: semant.c \
	../include/absyn.h ../include/basic_types.h ../include/error.h \
	../include/list.h ../include/memory.h ../include/symtab.h

symtab.o: symtab.c \
	../include/basic_types.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/symtab.h

symtab_printer.o: symtab_printer.c \
	../include/basic_types.h ../include/config.h ../include/semant.h \
	../include/symtab.h

tokenize.o: tokenize.c \
	../include/basic_types.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/tokens.h
