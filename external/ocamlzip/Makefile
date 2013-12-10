### Configuration section

# The name of the Zlib library.  Usually -lz
ZLIB_LIB=-lz

# The directory containing the Zlib library (libz.a or libz.so)
ZLIB_LIBDIR=/usr/local/lib

# The directory containing the Zlib header file (zlib.h)
ZLIB_INCLUDE=/usr/local/include

OCAML_INCLUDE=$(shell ocamlc -where)

# Where to install the library.  By default: sub-directory 'zip' of
# OCaml's standard library directory.
INSTALLDIR=`$(OCAMLC) -where`/zip

### End of configuration section

OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib

OBJS=zlib.cmo zip.cmo gzip.cmo
C_OBJS=zlibstubs.o

all: libcamlzip.a zip.cma

allopt: libcamlzip.a zip.cmxa

opt: allopt
all.opt: allopt

zip.cma: $(OBJS)
	$(OCAMLMKLIB) -o zip -oc camlzip $(OBJS) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB)

zip.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLMKLIB) -o zip -oc camlzip $(OBJS:.cmo=.cmx) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB)

libcamlzip.a: $(C_OBJS)
	$(OCAMLMKLIB) -oc camlzip $(C_OBJS) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB)

.SUFFIXES: .mli .ml .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<
.c.o:
	$(OCAMLC) -c -ccopt -g -ccopt -I$(ZLIB_INCLUDE) -ccopt -I$(OCAML_INCLUDE) $<

clean:
	rm -f *.cm*
	rm -f *.o *.a
	rm -f *.so

install:
	mkdir -p $(INSTALLDIR)
	cp zip.cma zip.cmi gzip.cmi zip.mli gzip.mli libcamlzip.a $(INSTALLDIR)
	if test -f dllcamlzip.so; then \
	  cp dllcamlzip.so $(INSTALLDIR); \
          ldconf=`$(OCAMLC) -where`/ld.conf; \
          installdir=$(INSTALLDIR); \
          if test `grep -s -c $$installdir'$$' $$ldconf || :` = 0; \
          then echo $$installdir >> $$ldconf; fi \
        fi

installopt:
	cp zip.cmxa zip.a zip.cmx gzip.cmx $(INSTALLDIR)

install-findlib:
	ocamlfind install zip META *.mli *.a *.cmi *.cma $(wildcard *.cmxa) $(wildcard *.so)

depend:
	gcc -MM -I$(ZLIB_INCLUDE) -I$(OCAML_INCLUDE) *.c > .depend
	ocamldep *.mli *.ml >> .depend

-include .depend
