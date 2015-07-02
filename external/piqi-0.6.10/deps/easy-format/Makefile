OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = easy_format

SOURCES = easy_format.ml


OCAMLFIND_PACKAGE = easy-format
LIBINSTALL_FILES = easy_format.cmo easy_format.cmx easy_format.o easy_format.cmi easy_format.mli


all: bcl ncl


install: libinstall


uninstall: libuninstall



download:
	$(MAKE) -f Makefile.download


clean::
	$(MAKE) -f Makefile.download clean


include $(OCAMLMAKEFILE)
