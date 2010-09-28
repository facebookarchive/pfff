TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= programming_language.ml \
     parse_info.ml ast_generic.ml meta_ast_generic.ml \
     place_code.ml archi_code.ml highlight_code.ml \
     archi_code_lexer.ml archi_code_parse.ml \
     tags_file.ml \
     pleac.ml \
     database_code.ml \
     statistics_code.ml statistics_parsing.ml \
     big_grep.ml \
     test_program_lang.ml

SYSLIBS= str.cma unix.cma 
LIBS=../commons/commons.cma
INCLUDEDIRS= ../commons ../commons/lib-sexp ../commons/lib-json

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



archi_code_lexer.ml: archi_code_lexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f archi_code_lexer.ml
beforedepend:: archi_code_lexer.ml
