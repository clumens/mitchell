GLOBAL_OPTS = -Werror

all: compiler

compiler:
	$(MAKE) -C src/kern

tags:
	( cd src && ctags -R )

test: test-compiler

test-compiler: compiler
	@passed=0 ; \
	failed=0 ; \
	for t in tests/*.mitchell; do \
		echo -n "$$(basename $$t)... " ; \
		LOCAL_OPTS="$$(grep Options $$t | cut -d':' -f2-)" ; \
		src/kern/mitchell $(GLOBAL_OPTS) $$LOCAL_OPTS $$t 1>&- 2>&- ; \
		retval=$$? ; \
		if [ $$retval -gt 128 ]; then \
			failed=$$(expr $$failed + 1) ; \
			echo "FAIL (CORE DUMPED)" ; \
		elif [ $$retval = 0 -a ! -z "$$(grep PASS $$t)" -o \
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
	$(MAKE) -C src/kern clean

distclean: clean
	-rm src/tags
