TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_html.ml \
     ast_html.ml \
     meta_ast_html.ml \
     visitor_html.ml \
     lib_parsing_html.ml \
     parser_html.ml lexer_html.ml \
     token_helpers_html.ml \
     dtd_simple.ml \
     parse_html.ml \
     unparse_html.ml \
     pretty_print_html.ml \
     unit_parsing_html.ml \
     test_parsing_html.ml



SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
 $(TOP)/globals \
 $(TOP)/h_program-lang

# $(TOP)/external/ocamlnet/netsys \
# $(TOP)/external/ocamlnet/netstring \

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


lexer_html.ml: lexer_html.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_html.ml
beforedepend:: lexer_html.ml


parser_html.ml parser_html.mli: parser_html.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_html.ml parser_html.mli parser_html.output
beforedepend:: parser_html.ml parser_html.mli


visitor_html.cmo: visitor_html.ml
	$(OCAMLC) -w y -c $<


##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_html
EXPORTSRC=meta_ast_html.mli \
   parser_html.mli parse_html.mli \
   lib_parsing_html.mli visitor_html.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
          ast_html.ml ast_html.cmi
