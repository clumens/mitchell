CC = gcc
CFLAGS = -O2 -Wall -I../include

OBJS = absyn_printer.o list.o main.o parse.o semant.o symtab.o \
		 symtab_printer.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^ -lgc -ldl

clean:
	-rm -f *.o mitchell
