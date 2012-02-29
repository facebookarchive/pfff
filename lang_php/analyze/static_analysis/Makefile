TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= \
  cyclomatic_php.ml layer_cyclomatic_php.ml \
  code_rank_php.ml \
  coverage_static_php.ml \
  deadcode_php.ml   layer_deadcode_php.ml \
  unit_static_analysis_simple_php.ml

#  dataflow_php_liveness.ml \
#  dataflow_php_array.ml \
#  tainted_php.ml \


SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/commons.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/commons/lib-json \
   $(TOP)/external/ocamlpcre/lib \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/lang_php/parsing \
   $(TOP)/lang_php/analyze/foundation \

#   $(TOP)/lang_php/analyze/database \

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


