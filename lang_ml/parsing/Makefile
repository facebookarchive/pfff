TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_ml.ml \
     ast_ml.ml \
     meta_ast_ml.ml \
     visitor_ml.ml lib_parsing_ml.ml \
     parser_ml.ml lexer_ml.ml \
     token_helpers_ml.ml \
     parse_ml.ml \
     test_parsing_ml.ml unit_parsing_ml.ml

SYSLIBS= str.cma unix.cma 

# globals.cma is used only for Config.patch in test_parse_ml
LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
  $(TOP)/globals \
  $(TOP)/h_program-lang \
  $(TOP)/matcher \

##############################################################################
# Generic variables
##############################################################################

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


lexer_ml.ml: lexer_ml.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_ml.ml
beforedepend:: lexer_ml.ml


parser_ml.ml parser_ml.mli: parser_ml.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_ml.ml parser_ml.mli parser_ml.output
beforedepend:: parser_ml.ml parser_ml.mli


visitor_ml.cmo: visitor_ml.ml
	$(OCAMLC) -w y -c $<

##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_ml
EXPORTSRC=meta_ast_ml.mli \
   parser_ml.mli parse_ml.mli \
   lib_parsing_ml.mli visitor_ml.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
          ast_ml.ml ast_ml.cmi flag_parsing_ml.ml flag_parsing_ml.cmi
