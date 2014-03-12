TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_js.ml \
     ast_js.ml \
     meta_ast_js.ml \
     visitor_js.ml lib_parsing_js.ml \
     parser_js.ml lexer_js.ml \
     token_helpers_js.ml \
     parsing_hacks_js.ml \
     parse_js.ml \
     test_parsing_js.ml unit_parsing_js.ml


SYSLIBS= str.cma unix.cma

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
# install
##############################################################################
LIBNAME=pfff-lang_js
EXPORTSRC=meta_ast_js.mli \
   parser_js.mli parse_js.mli \
   lib_parsing_js.mli visitor_js.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
          ast_js.ml ast_js.cmi ast_js.cmo ast_js.cmx
