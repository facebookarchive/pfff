TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

# those files used to be in analyze/basic/
SRC= flag_analyze_php.ml \
     entity_php.ml \
     namespace_php.ml scoping_php.ml \
     comment_php.ml annotation_php.ml \
     unsugar_php.ml \
     env_php.ml \
     class_php.ml include_require_php.ml  \
# used to be in ast_simple/
SRC+= ast_php_simple.ml ast_php_simple_build.ml meta_ast_php_simple.ml
SRC+= pil.ml
#note: tools/phpmanual_xml.ml is related to builtins_php.ml
SRC+= defs_uses_php.ml \
      builtins_php.ml \
      controlflow_php.ml  controlflow_build_php.ml \
      dataflow_php.ml \
      callgraph_php2.ml \
# used to be in facebook/static_analysis/
SRC+= env_interpreter_php.ml \
      abstract_interpreter_php_helpers.ml \
      tainting_fake_php.ml tracing_php.ml abstract_interpreter_php.ml \
      callgraph_php_build.ml
# used to be in facebook/static_analysis/
SRC+= dependencies_toposort_php.ml \
      env_typing_php.ml typing_helpers_php.ml\
      builtins_typed_php.ml typing_unify_php.ml \
      typing_php.ml
SRC+= database_juju_php.ml
SRC+= graph_code_php.ml
# used to be in analyze/
SRC+= tags_php.ml \
     database_light_php.ml \
     database_prolog_php.ml \
     statistics_php.ml \
     pretty_print_php_simple.ml \
     unit_foundation_php.ml \
     unit_static_analysis_php.ml   unit_typeinfer_php.ml \
     unit_prolog_php.ml

#old: pil is now subsumed by ast_simple and the abstract interpreter
# pil.ml pretty_print_pil.ml controlflow_pil.ml \
# meta_pil.ml visitor_pil.ml \
# pil_build.ml controlflow_build_pil.ml \
# cfa_pil.ml \
# dataflow_php.ml dataflow_pil.ml \
# aliasing_function_php.ml bottomup_analysis_php.ml

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/lang_ml/parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/external/jsonwheel \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/graph_code \
   $(TOP)/globals \
   $(TOP)/matcher \
   $(TOP)/lang_php/parsing \
   $(TOP)/lang_ml/parsing \

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

visitor_pil.cmo: visitor_pil.ml
	$(OCAMLC) -w y -c $<

##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_php-analyze-foundation

EXPORTSRC=\
  abstract_interpreter_php.mli \
  annotation_php.mli \
  ast_php_simple_build.mli \
  builtins_php.mli \
  callgraph_php2.mli \
  callgraph_php_build.mli \
  class_php.mli \
  comment_php.mli \
  controlflow_build_php.mli \
  controlflow_php.mli \
  database_juju_php.mli \
  database_light_php.mli \
  database_prolog_php.mli \
  defs_uses_php.mli \
  entity_php.mli \
  env_php.mli \
  graph_code_php.mli \
  include_require_php.mli \
  meta_ast_php_simple.mli \
  namespace_php.mli \
  pretty_print_php_simple.mli \
  scoping_php.mli \
  statistics_php.mli \
  tags_php.mli \
  tracing_php.mli \
  typing_php.mli \
  typing_unify_php.mli \
  unit_foundation_php.mli \
  unit_prolog_php.mli \
  unit_static_analysis_php.mli \
  unit_typeinfer_php.mli \
  unsugar_php.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) ast_php_simple.cmi \
          env_interpreter_php.cmi abstract_interpreter_php_helpers.cmi \
	  $(SRC)
