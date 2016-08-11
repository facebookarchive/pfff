TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= flag_matcher_php.ml   \
     refactoring_code_php.ml \
     metavars_php.ml \
     php_vs_php.ml \
     matching_php.ml \
     transforming_php.ml \
     sgrep_php.ml \
     spatch_php.ml \
     unit_matcher_php.ml \

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocollection \
   $(TOP)/commons/ocamlextra \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/external/ \
   $(TOP)/matcher \
   ../parsing

##############################################################################
# Generic variables
##############################################################################
#BUG in ocamlc, false positive on unused value
#WARNING_FLAGS=-w +A-4-29-6-45-32
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top

##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_php-matcher

EXPORTSRC=\
  matching_php.mli \
  metavars_php.mli \
  php_vs_php.mli \
  refactoring_code_php.mli \
  sgrep_php.mli \
  spatch_php.mli \
  transforming_php.mli \
  unit_matcher_php.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
	  $(SRC)
