CC = gcc
CFLAGS = -O2 -Wall -I../include

OBJS = main.o parse.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^ -lgc

clean:
	-rm *.o mitchell
