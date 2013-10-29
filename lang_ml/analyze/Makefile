TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= module_ml.ml \
     graph_code_ml.ml \
     tags_ml.ml \
     database_light_ml.ml \
     ast_cmt.ml meta_ast_cmt.ml \
     graph_code_cmt_helpers.ml graph_code_cmt.ml \
     coverage_ml.ml \
     test_analyze_ml.ml unit_analyze_ml.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     $(TOP)/h_version-control/lib.cma \
     ../parsing/lib.cma \

MAKESUBDIRS=visual

OCAMLCOMPILERDIR=$(shell ocamlc -where)/compiler-libs

INCLUDEDIRS= $(TOP)/commons \
   $(OCAMLCOMPILERDIR) \
   $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/commons/lib-json \
   $(TOP)/globals \
   $(TOP)/external/ \
   $(TOP)/h_program-lang \
   $(TOP)/h_version-control  \
   $(TOP)/h_visualization  \
   $(TOP)/globals \
   ../parsing \
   visual \

#   $(TOP)/analyze_php/basic \
#   $(TOP)/analyze_php/foundation \
#   $(TOP)/analyze_php/database \
#   $(TOP)/analyze_php/checker \
#   $(TOP)/analyze_php/annotaters \
#   $(TOP)/analyze_php/tools \
#   $(TOP)/analyze_php/typing \
#   $(TOP)/analyze_php/static_analysis \
#   $(TOP)/analyze_php/dynamic_analysis \
#   $(TOP)/analyze_php/qa_test \

#   $(PCREINCLUDE) $(JANEINCLUDE)
#$(TOP)/matcher_php
#$(TOP)/parsing-comments \
#$(TOP)/smpl \

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
# no rec yet
all:: rec $(TARGET).cma
all.opt:: rec.opt $(TARGET).cmxa

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 
depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done 


$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


PP=-pp "cpp -DOCAML_VERSION=$(OCAMLVERSION)"

meta_ast_cmt.cmo: meta_ast_cmt.ml
	$(OCAMLC) $(PP)  -c $<

meta_ast_cmt.cmx: meta_ast_cmt.ml
	$(OCAMLOPT) $(PP) -c $<

graph_code_cmt.cmo: graph_code_cmt.ml
	$(OCAMLC) $(PP)  -c $<

graph_code_cmt.cmx: graph_code_cmt.ml
	$(OCAMLOPT) $(PP) -c $<

##############################################################################
# Literate Programming rules
##############################################################################
