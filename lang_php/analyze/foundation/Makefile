TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

# used to be in analyze/basic/
SRC= flag_analyze_php.ml   \
     entity_php.ml \
     namespace_php.ml \
     comment_php.ml annotation_php.ml \
     unsugar_php.ml \
     scoping_php.ml \
     env_php.ml \
     include_require_php.ml define_php.ml func_php.ml class_php.ml \
     lib_analyze_php.ml \

# used to be in ast_simple/
SRC+=ast_php_simple.ml ast_php_simple_build.ml meta_ast_php_simple.ml \
     ast_php_simple_toposort.ml

#old: pil, subsumed by ast_simple and abstract interpreter
#     pil.ml pretty_print_pil.ml controlflow_pil.ml \
#     meta_pil.ml visitor_pil.ml \
#     pil_build.ml controlflow_build_pil.ml \
#     cfa_pil.ml
#     dataflow_php.ml dataflow_pil.ml \
#     aliasing_function_php.ml bottomup_analysis_php.ml

#note: tools/phpmanual_xml.ml is related to builtins_php.ml
SRC+= defs_uses_php.ml \
      builtins_php.ml \
      controlflow_php.ml  controlflow_build_php.ml \
      callgraph_php.ml \
      database_php.ml \
      graph_php.ml \

# used to be in analyze/
SRC+= tags_php.ml normalize_php.ml \
     database_light_php.ml
SRC+= unit_foundation_php.ml

SRC+= database_php_build_helpers.ml database_php_build.ml \
      database_php_build2.ml
SRC+= database_prolog_php.ml unit_prolog_php.ml
SRC+= unit_analyze_db_php.ml


SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/commons.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/commons/lib-json $(TOP)/commons/lib-xml \
   $(TOP)/external/ocamlpcre/lib \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/lang_php/parsing \

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
