TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_js.ml \
     ast_js.ml \
     meta_ast_js.ml \
     export_ast_js.ml \
     visitor_js.ml lib_parsing_js.ml \
     map_js.ml \
     parser_js.ml lexer_js.ml \
     token_helpers_js.ml \
     parse_js.ml \
     unparse_js.ml \
     test_parsing_js.ml unit_parsing_js.ml


SYSLIBS= str.cma unix.cma 

# globals.cma is used only for Config.patch in test_parse_js
LIBS=$(TOP)/commons/commons.cma \
 $(TOP)/globals/globals.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
  $(TOP)/commons/lib-sexp $(TOP)/commons/lib-json \
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


lexer_js.ml: lexer_js.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_js.ml
beforedepend:: lexer_js.ml


parser_js.ml parser_js.mli: parser_js.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_js.ml parser_js.mli parser_js.output
beforedepend:: parser_js.ml parser_js.mli


visitor_js.cmo: visitor_js.ml
	$(OCAMLC) -w y -c $<

##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
