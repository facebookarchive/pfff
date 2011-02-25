OCAMLMAKEFILE = ../OCamlMakefile

SOURCES := pcre.mli pcre.ml pcre_stubs.c
CFLAGS := -O2 $(shell pcre-config --cflags)
PCRE_FLAGS := $(shell pcre-config --libs)
OCAMLLDFLAGS := -cclib "$(PCRE_FLAGS)"
OCAMLMKLIB_FLAGS := $(PCRE_FLAGS)
RESULT := pcre

all:

install: libinstall
uninstall: libuninstall

-include $(OCAMLMAKEFILE)
