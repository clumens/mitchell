all:
	@echo No default target at this time.

tags:
	( cd src && ctags -R )

# Targets for testing.
test: test-compiler

test-compiler: compiler
	@LANG=en_US.UTF-8 run-tests.sh

# Targets for cleaning.
distclean:
	-rm src/tags
