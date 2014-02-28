TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_opa.ml \
     ast_opa.ml \
     lib_parsing_opa.ml \
     parser_opa.ml lexer_opa.ml \
     token_helpers_opa.ml \
     token_views_opa.ml ast_fuzzy_opa.ml meta_ast_fuzzy_opa.ml \
     parse_opa.ml \
     visitor_opa.ml \
     unit_parsing_opa.ml test_parsing_opa.ml

# meta_ast_opa.ml export_ast_opa.ml \
# unparse_opa.ml \

SYSLIBS= str.cma unix.cma 

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
  $(TOP)/globals \
  $(TOP)/h_program-lang \

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


lexer_opa.ml: lexer_opa.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_opa.ml
beforedepend:: lexer_opa.ml


parser_opa.ml parser_opa.mli: parser_opa.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_opa.ml parser_opa.mli parser_opa.output
beforedepend:: parser_opa.ml parser_opa.mli


visitor_opa.cmo: visitor_opa.ml
	$(OCAMLC) -w y -c $<

##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
