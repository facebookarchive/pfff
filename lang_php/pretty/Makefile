TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
BIN=prettyphp

SRC= \
  ast_pp.ml ast_pp_build.ml \
  pretty_print.ml\
  unparse_pretty_print_mix.ml \
  unit_pretty_print_php.ml

LIBS= \
  $(TOP)/commons/lib.cma \
  $(TOP)/h_program-lang/lib.cma \
  $(TOP)/globals/lib.cma \
  $(TOP)/matcher/lib.cma \
  $(TOP)/lang_php/parsing/lib.cma \

SYSLIBS=bigarray.cma nums.cma str.cma unix.cma
SYSLIBSOPT = $(SYSLIBS:.cma=.cmxa)

INCLUDEDIRS= \
  $(TOP)/commons \
  $(TOP)/commons/ocamlextra \
  $(TOP)/globals \
  $(TOP)/h_program-lang \
  $(TOP)/lang_php/parsing \

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(BIN)
all.opt:: $(TARGET).cmxa $(BIN).opt

$(BIN): $(OBJS) $(LIBS) main.ml
	$(OCAMLC) -o $@ -custom $(SYSLIBS) $(LIBS) $(OBJS) main.ml

$(BIN).opt: $(OBJS) $(LIBS) main.ml
	$(OCAMLOPT) -o $@ $(SYSLIBSOPT) $(LIBS:.cma=.cmxa) $(OPTOBJS) main.ml

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)

clean::
	rm -f $(BIN) $(BIN).opt

##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_php-pretty

EXPORTSRC=\
  ast_pp_build.mli \
  pretty_print.mli \
  unit_pretty_print_php.mli \
  unparse_pretty_print_mix.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
	  $(SRC) ast_pp.cmi
