
all opt doc install clean :
	$(MAKE) -C src $@


DISTSRC = aclocal.m4 config.make.in configure configure.ac Makefile Makefile.rules \
          doc support/install-sh support/ocaml.m4 \
          src/*.ml src/*.mli src/*.c src/*.h src/Makefile src/.depend_c \
          test/Makefile test/*.ml
dist : doc
	export DIRNAME=$${PWD##*/} && \
	cd .. && mv $$DIRNAME cairo-ocaml-$(VERSION) && \
	tar zcvf cairo-ocaml-$(VERSION).tar.gz $(addprefix cairo-ocaml-$(VERSION)/,$(DISTSRC)) && \
        mv cairo-ocaml-$(VERSION) $$DIRNAME


#configure : configure.ac
#	aclocal -I support
#	autoconf
config.make : config.make.in configure
	$(error run ./configure)
include config.make

install_findlib:
	ocamlfind install cairo META src/*.cm* src/*.so
.PHONY : all opt doc install clean

depend:

all.opt: opt
