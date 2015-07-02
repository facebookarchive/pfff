OCAMLMAKEFILE := ../OCamlMakefile


RESULT = optcomp

SOURCES = pa_optcomp.ml


OCAMLFIND_PACKAGE = optcomp

LIBINSTALL_FILES = optcomp.cma pa_optcomp.cmi


INCDIRS = +camlp4


all: bcl


install: libinstall


uninstall: libuninstall

depend::

download:
	#$(MAKE) -f Makefile.download


clean::
	#$(MAKE) -f Makefile.download clean


include $(OCAMLMAKEFILE)
