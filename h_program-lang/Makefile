TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= entity_code.ml scope_code.ml \
     parse_info.ml comment_code.ml \
     ast_fuzzy.ml meta_ast_generic.ml \
     highlight_code.ml \
     archi_code.ml archi_code_lexer.ml archi_code_parse.ml \
     skip_code.ml \
     refactoring_code.ml \
     database_code.ml \
     prolog_code.ml datalog_code.ml \
     layer_code.ml \
     tags_file.ml \
     errors_code.ml \
     layer_parse_errors.ml \
     coverage_code.ml layer_coverage.ml \
     overlay_code.ml big_grep.ml pleac.ml info_code.ml \
     pretty_print_code.ml \
     test_program_lang.ml unit_program_lang.ml

# See also graph_code/graph_code.ml! closely related to h_program-lang/

SYSLIBS= str.cma unix.cma
LIBS=../commons/lib.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/commons/ocamlextra \
 $(TOP)/external/jsonwheel \
 $(TOP)/h_files-format

# other sources:
#  prolog_code.pl, facts.pl, for the prolog-based code query engine

# dead: visitor_code, statistics_code, programming-language, ast_generic
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

##############################################################################
# install
##############################################################################
LIBNAME=pfff-h_program-lang
EXPORTSRC=\
  archi_code.mli \
  archi_code_parse.mli \
  ast_fuzzy.mli \
  big_grep.mli \
  comment_code.mli \
  coverage_code.mli \
  database_code.mli \
  datalog_code.mli \
  entity_code.mli \
  errors_code.mli \
  highlight_code.mli \
  info_code.mli \
  layer_code.mli \
  layer_coverage.mli \
  layer_parse_errors.mli \
  meta_ast_generic.mli \
  overlay_code.mli \
  parse_info.mli \
  pleac.mli \
  prolog_code.mli \
  refactoring_code.mli \
  scope_code.mli \
  skip_code.mli \
  tags_file.mli \
  test_program_lang.mli \
  unit_program_lang.mli \

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
	  pretty_print_code.cmi
