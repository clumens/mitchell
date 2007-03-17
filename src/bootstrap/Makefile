all:

heap:
	mkdir -p obj/
	ml-build sources.cm Main.main obj/mitchell

clean:
	-rm dump.dot mitchell.grm.sml mitchell.grm.tex mitchell.lex.sml
	-rm -r obj

realclean: clean
	-rm -r .cm env/.cm getopt/.cm lib/.cm test/.cm
