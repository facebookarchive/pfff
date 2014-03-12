TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_css.ml \
     ast_css.ml \
     meta_ast_css.ml \
     lib_parsing_css.ml \
     export_ast_css.ml \
     parser_css.ml lexer_css.ml \
     token_helpers_css.ml \
     parse_css.ml \
     test_parsing_css.ml

#     visitor_html.ml \
#     unparse_html.ml \
     

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma

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


lexer_css.ml: lexer_css.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_css.ml
beforedepend:: lexer_css.ml


parser_css.ml parser_css.mli: parser_css.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_css.ml parser_css.mli parser_css.output
beforedepend:: parser_css.ml parser_css.mli


visitor_css.cmo: visitor_css.ml
	$(OCAMLC) -w y -c $<

##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_css
EXPORTSRC=meta_ast_css.mli \
   parser_css.mli parse_css.mli \
   lib_parsing_css.mli

# visitor_css.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
          ast_css.ml ast_css.cmi
