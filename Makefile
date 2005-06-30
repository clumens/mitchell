all: compiler

compiler:
	$(MAKE) -C src/kern

tags:
	( cd src && ctags -R )

test: test-compiler

test-compiler: compiler
	@run-tests.sh

clean:
	$(MAKE) -C src/kern clean

distclean: clean
	-rm src/tags
