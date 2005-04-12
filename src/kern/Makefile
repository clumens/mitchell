# $Id: Makefile,v 1.25 2005/04/12 01:13:01 chris Exp $

CC = gcc
BASE_CFLAGS = -O2 -Wall -I../include
LDFLAGS = -lgc -ldl

ifdef DEBUG
	override BASE_CFLAGS += -g
	override CFLAGS += $(subst -O2,,$(BASE_CFLAGS))
else
	override CFLAGS += $(BASE_CFLAGS)
endif

OBJS = absyn_printer.o desugar.o desugar_case.o desugar_decls.o list.o \
		 main.o parse.o rename.o semant.o str.o symtab.o symtab_printer.o \
		 tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	-rm -f *.o mitchell

#####################################################################
# Dependancies                                                      #
#####################################################################
absyn_printer.o: absyn_printer.c \
	../include/absyn.h ../include/config.h ../include/basic_types.h \
	../include/error.h ../include/list.h

desugar.o: desugar.c \
	../include/absyn.h ../include/config.h ../include/desugar.h \
	../include/error.h ../include/memory.h ../include/str.h

desugar_case.o: desugar_case.c \
	../include/absyn.h ../include/basic_types.h ../include/desugar.h \
	../include/error.h ../include/list.h ../include/memory.h ../include/str.h

desugar_decls.o: desugar_decls.c \
	../include/absyn.h ../include/basic_types.h ../include/desugar.h \
	../include/list.h ../include/memory.h ../include/str.h

list.o: list.c \
	../include/list.h ../include/memory.h

main.o: main.c \
	../include/absyn.h ../include/config.h ../include/desugar.h \
	../include/error.h ../include/memory.h ../include/parse.h \
	../include/semant.h ../include/version.h

parse.o: parse.c \
	../include/absyn.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/str.h ../include/tokens.h

rename.o: rename.c \
	../include/absyn.h ../include/desugar.h ../include/error.h \
	../include/list.h ../include/str.h

semant.o: semant.c \
	../include/absyn.h ../include/basic_types.h ../include/error.h \
	../include/list.h ../include/memory.h ../include/str.h ../include/symtab.h

str.o: str.c \
	../include/basic_types.h ../include/memory.h ../include/str.h

symtab.o: symtab.c \
	../include/basic_types.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/symtab.h

symtab_printer.o: symtab_printer.c \
	../include/basic_types.h ../include/config.h ../include/semant.h \
	../include/symtab.h

tokenize.o: tokenize.c \
	../include/basic_types.h ../include/config.h ../include/error.h \
	../include/memory.h ../include/tokens.h
