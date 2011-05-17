TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_cpp.ml \
     token_cpp.ml ast_cpp.ml \
     meta_ast_cpp.ml \
     export_ast_cpp.ml \
     visitor_cpp.ml lib_parsing_cpp.ml \
     type_cpp.ml \
     semantic_cpp.ml \
     lexer_parser_cpp.ml \
     parser_cpp.ml lexer_cpp.ml \
     parsing_stat_cpp.ml \
     token_helpers_cpp.ml \
     parsing_hacks.ml \
     parsing_recovery_cpp.ml parsing_consistency_cpp.ml \
     parse_cpp.ml \
     test_parsing_cpp.ml \

# parser_php_mly_helper.ml \

#todo:  empty for now
SRC+=pp_analysis.ml \
  pp_ast.ml pp_token.ml token_cpp.ml token_views_cpp.ml

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/commons.cma \
   $(TOP)/h_program-lang/lib.cma
INCLUDEDIRS= \
 $(TOP)/commons $(TOP)/commons/lib-sexp $(TOP)/commons/lib-json \
  $(TOP)/h_program-lang

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


lexer_cpp.ml: lexer_cpp.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_cpp.ml
beforedepend:: lexer_cpp.ml


parser_cpp.ml parser_cpp.mli: parser_cpp.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_cpp.ml parser_cpp.mli parser_cpp.output
beforedepend:: parser_cpp.ml parser_cpp.mli


visitor_cpp.cmo: visitor_cpp.ml
	$(OCAMLC) -w y -c $<

parsing_hacks.cmo: parsing_hacks.ml
	$(OCAMLC) -w -9 -c $<

##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
