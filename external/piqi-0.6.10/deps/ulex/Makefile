OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = ulexing

SOURCES = cset.ml utf8.ml ulexing.ml utf16.ml ulex.ml


OCAMLFIND_PACKAGE = ulex

MODS = ulexing utf16 utf8
LIBINSTALL_FILES = \
	$(wildcard $(MODS:=.mli) $(MODS:=.cmi) $(MODS:=.cmx) pa_ulex.cma ulexing.a ulexing.cma ulexing.cmxa)


all: bcl ncl pa_ulex.cma


install: libinstall


uninstall: libuninstall


pa_ulex.cma:
	$(MAKE) -f Makefile.camlp4


download:
	$(MAKE) -f Makefile.download


clean::
	$(MAKE) -f Makefile.camlp4 clean
	$(MAKE) -f Makefile.download clean


include $(OCAMLMAKEFILE)
