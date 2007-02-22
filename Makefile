all:

compiler:
	$(MAKE) -C src/bootstrap heap
	sed -e 's|###HEAPFILE###|src/bootstrap/obj/mitchell.$(shell uname -m)-linux|' mitchell.in > mitchell
	chmod 755 mitchell

# Targets for testing.
test: compiler

# Targets for cleaning.
clean:
	-rm mitchell
	$(MAKE) -C src/bootstrap clean
