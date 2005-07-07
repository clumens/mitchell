# $Id: Makefile,v 1.7 2005/07/07 05:04:02 chris Exp $

include Includes.mk

all: compiler

# Targets for building.
compiler:
	$(MAKE) -C src/kern

tags:
	( cd src && ctags -R )

# Targets for testing.
test: test-compiler

test-compiler: compiler
	@LANG=en_US.UTF-8 run-tests.sh

# Targets for cleaning.
clean:
	$(MAKE) -C src/kern clean

distclean: clean
	-rm src/tags
