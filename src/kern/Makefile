CC = gcc
CFLAGS = -O2 -Wall -I../include -g

OBJS = absyn_printer.o main.o parse.o semant.o symtab.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^ -lgc -ldl

clean:
	-rm -f *.o mitchell
