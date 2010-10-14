
# This file is intentionnaly not using ../Makefile.common.
# This is to show how to build a simple Makefile from scratch
# to people who wants to use pfff in their own application.

##############################################################################
# Variables
##############################################################################

include ../Makefile.config
ifeq ($(FEATURE_BDB),1)
ANALYZECMA=../external/ocamlpcre/lib/pcre.cma \
	   ../external/ocamlbdb/bdb.cma ../commons/commons_bdb.cma \
           ../lang_php/analyze/lib.cma \

ANALUZEINCLUDES= \
      ../external/ocamlbdb/ \
      ../external/ocamlpcre/lib \
      ../h_program-lang \
      ../lang_php/analyze
endif

LIBS= ../commons/commons.cma \
      ../h_version-control/lib.cma \
      ../h_program-lang/lib.cma \
      ../lang_php/parsing/lib.cma \
      $(ANALYZECMA)
INCLUDEDIRS=../commons ../commons/lib-sexp ../commons/lib-json \
            ../lang_php/parsing $(ANALUZEINCLUDES)

SYSLIBS= str.cma unix.cma nums.cma bigarray.cma


# If you want to embed pfff in your own application, uncomment the following
# instead of the previous settings
#MAKESUBDIRS=commons parsing_php 
#LIBS=commons/commons.cma parsing_php/parsing_php.cma
#INCLUDEDIRS=commons/ parsing_php/
#rec::
#	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 
#clean::
#	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean || exit 1; done 
##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS=-g -dtypes # -w A

# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=


OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES) -thread
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) -thread
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES) -thread

##############################################################################
# Top rules
##############################################################################

# some of the ml files are generated from the literate document in parsing_php/
# so dont forget to do a 'make sync' in parsing_php/ before doing make
# in this directory.
PROGS=php_transducer.byte \
 show_function_calls1.byte show_function_calls2.byte show_function_calls3.byte \
 justin.byte bjacobs.byte
# simple_zero_to_null.byte count_elseifs.byte

#ifeq ($(FEATURE_BDB),1)
#PROGS+=kaplan.byte
#endif

all: rec  $(PROGS)
opt: $(PROGS:.byte=.opt)
all.opt: opt

rec::

.SUFFIXES: .byte .opt

%.byte: %.cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $(LIBS) $^

%.opt: %.cmx
	$(OCAMLOPT) -o $@ $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa)  $^


clean::
	rm -f *.opt *.byte

##############################################################################
# Developer rules
##############################################################################

tags:
	otags -no-mli-tags -r  .

clean::
	rm -f TAGS

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC)  -c $<
.mli.cmi:
	$(OCAMLC)  -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

.ml.mldepend: 
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[iox] *.o *.annot
clean::
	rm -f *~ .*~ *.exe #*#

distclean::
	rm -f .depend

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
