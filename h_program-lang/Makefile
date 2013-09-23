TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= parse_info.ml \
     ast_fuzzy.ml \
     meta_ast_generic.ml \
     scope_code.ml \
     highlight_code.ml \
     archi_code.ml archi_code_lexer.ml archi_code_parse.ml \
     skip_code.ml \
     refactoring_code.ml \
     layer_code.ml database_code.ml \
     graph_code.ml graph_code_opti.ml dependencies_matrix_code.ml \
     graph_code_prolog.ml tags_file.ml graph_code_tags.ml \
     graph_code_database.ml \
     graph_code_analysis.ml layer_graph_code.ml \
     statistics_parsing.ml layer_parse_errors.ml \
     coverage_code.ml layer_coverage.ml \
     overlay_code.ml big_grep.ml pleac.ml info_code.ml \
     pretty_print_code.ml \
     test_program_lang.ml unit_program_lang.ml

SYSLIBS= str.cma unix.cma
LIBS=../commons/lib.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/commons/lib-sexp $(TOP)/commons/lib-json \
 $(TOP)/commons/ocamlextra \
 $(TOP)/external/ocamlgraph $(TOP)/external/ocamlgraph/src \
 $(TOP)/h_files-format

# other sources:
#  database_code.pl, facts.pl, for the prolog-based code query engine

# dead: visitor_code, statistics_code, programming-language,
#  ast_generic
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
