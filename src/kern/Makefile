CC = gcc
CFLAGS = -Wall -I../include

OBJS = main.o parse.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^

tags:
	( cd .. && ctags -R )

clean:
	-rm mitchell *.o ../tags
