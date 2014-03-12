TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= flag_parsing_java.ml \
     ast_java.ml meta_ast_java.ml \
     lib_parsing_java.ml \
     parser_java.ml lexer_java.ml \
     token_helpers_java.ml \
     parsing_hacks_java.ml \
     parse_java.ml \
     visitor_java.ml \
     test_parsing_java.ml unit_parsing_java.ml

SYSLIBS= str.cma unix.cma

LIBS= $(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
  $(TOP)/h_program-lang \
  $(TOP)/matcher \
  $(TOP)/globals \

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


lexer_java.ml: lexer_java.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_java.ml
beforedepend:: lexer_java.ml


parser_java.ml parser_java.mli: parser_java.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_java.ml parser_java.mli parser_java.output
beforedepend:: parser_java.ml parser_java.mli

visitor_java.cmo: visitor_java.ml
	$(OCAMLC) -w y -c $<

##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_java
EXPORTSRC=meta_ast_java.mli \
   parser_java.mli parse_java.mli \
   lib_parsing_java.mli visitor_java.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
          ast_java.ml ast_java.cmi
