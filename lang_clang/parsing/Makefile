TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_clang.ml \
     visitor_clang.ml \
     meta_ast_clang.ml \
     lib_parsing_clang.ml \
     parser_clang.ml lexer_clang.ml \
     ast_clang.ml \
     parse_clang.ml \
     test_parsing_clang.ml

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
 $(TOP)/external/jsonwheel \
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


lexer_clang.ml: lexer_clang.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_clang.ml
beforedepend:: lexer_clang.ml


parser_clang.ml parser_clang.mli: parser_clang.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_clang.ml parser_clang.mli parser_clang.output
beforedepend:: parser_clang.ml parser_clang.mli


visitor_clang.cmo: visitor_clang.ml
	$(OCAMLC) -w y -c $<


##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
