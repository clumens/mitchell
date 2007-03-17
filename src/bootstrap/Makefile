SUBDIRS = env getopt lib parse semant

all:

heap:
	mkdir -p obj/
	ml-build sources.cm Main.main obj/mitchell

clean:
	-rm parse/dump.dot parse/mitchell.grm.sml parse/mitchell.grm.tex
	-rm parse/mitchell.lex.sml
	-rm -r obj

realclean: clean
	-rm -r .cm ; \
	for dir in ${SUBDIRS}; do \
		rm -r $$dir/.cm ; \
	done
