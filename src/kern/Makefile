CC = gcc
CFLAGS = -O2 -Wall -I../include

OBJS = main.o parse.o tokenize.o

all: mitchell

mitchell: $(OBJS)
	gcc $(CFLAGS) -o $@ $^

tags:
	( cd .. && ctags -R )

test: mitchell
	@passed=0 ; \
	failed=0 ; \
	for t in ../../tests/*.mitchell; do \
		echo -n "$$(basename $$t)... " ; \
		mitchell $$t 1>&- 2>&- ; \
		retval=$$? ; \
		if [ $$retval = 0 -a ! -z "$$(grep PASS $$t)" -o \
		     $$retval = 1 -a ! -z "$$(grep FAIL $$t)" ]; then \
			passed=$$(expr $$passed + 1) ; \
			echo "PASS" ; \
		else \
			failed=$$(expr $$failed + 1) ; \
			echo "FAIL" ; \
		fi \
	done ; \
	echo -e "----------\n$$passed tests passed\n$$failed tests failed"

clean:
	-rm mitchell *.o ../tags
