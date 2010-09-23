TOP=../../../
-include $(TOP)/Makefile.config

OCAMLMAKEFILE = ../OCamlMakefile

SOURCES   = pcre.mli pcre.ml pcre_stubs.c
CFLAGS    = -O2  -I $(PCRE_INCLUDE)

CLIBS	  = pcre
RESULT    = pcre

all:

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
