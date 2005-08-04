# $Id: Makefile,v 1.34 2005/08/04 03:15:40 chris Exp $

include ../../Includes.mk

CC = gcc
override CFLAGS += -O2 -Wall -I../include -DPACKAGE=\"mitchell\" \
	-DLOCALEDIR=\"$(LOCALEDIR)\" -fno-strict-aliasing
override LDFLAGS += -lgc -ldl

ifdef DEBUG
	override CFLAGS += -g
endif

OBJS = absyn_printer.o absyn_walk.o desugar.o desugar_case.o desugar_decls.o \
	error.o lift.o list.o main.o parse.o semant.o str.o symtab.o \
	symtab_printer.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	-rm -f *.o mitchell core

#####################################################################
# Dependancies                                                      #
#####################################################################
absyn_printer.o: absyn_printer.c \
	../include/absyn.h ../include/config.h ../include/basic_types.h \
	../include/error.h ../include/list.h ../include/translate.h

absyn_walk.o: absyn_walk.c \
	../include/absyn.h ../include/absyn_walk.h ../include/error.h \
	../include/list.h ../include/memory.h ../include/translate.h

desugar.o: desugar.c \
	../include/absyn.h ../include/absyn_walk.h ../include/config.h \
	../include/desugar.h ../include/error.h ../include/memory.h \
	../include/translate.h

desugar_case.o: desugar_case.c \
	../include/absyn.h ../include/basic_types.h ../include/desugar.h \
	../include/error.h ../include/list.h ../include/memory.h ../include/str.h \
	../include/translate.h

desugar_decls.o: desugar_decls.c \
	../include/absyn.h ../include/basic_types.h ../include/desugar.h \
	../include/list.h ../include/memory.h ../include/str.h ../include/symtab.h \
	../include/translate.h

error.o: error.c \
	../include/config.h ../include/error.h ../include/translate.h

lift.o: lift.c \
	../include/absyn.h ../include/desugar.h ../include/error.h \
	../include/list.h ../include/translate.h

list.o: list.c \
	../include/list.h ../include/memory.h

main.o: main.c \
	../include/absyn.h ../include/config.h ../include/desugar.h \
	../include/error.h ../include/memory.h ../include/parse.h \
	../include/semant.h ../include/translate.h ../include/version.h

parse.o: parse.c \
	../include/absyn.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/str.h ../include/tokens.h \
	../include/translate.h

semant.o: semant.c \
	../include/absyn.h ../include/basic_types.h ../include/error.h \
	../include/list.h ../include/memory.h ../include/str.h ../include/symtab.h \
	../include/translate.h

str.o: str.c \
	../include/basic_types.h ../include/memory.h ../include/str.h

symtab.o: symtab.c \
	../include/basic_types.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/symtab.h ../include/translate.h

symtab_printer.o: symtab_printer.c \
	../include/basic_types.h ../include/config.h ../include/semant.h \
	../include/symtab.h ../include/translate.h

tokenize.o: tokenize.c \
	../include/basic_types.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/tokens.h ../include/translate.h
