CC = gcc
CFLAGS = -O2 -Wall -I../include
LDFLAGS = -lgc -ldl

ifdef DEBUG
	override CFLAGS += -g
endif

OBJS = absyn_printer.o list.o main.o parse.o semant.o symtab.o \
		 symtab_printer.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	-rm -f *.o mitchell
