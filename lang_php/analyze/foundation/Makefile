TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= defs_uses_php.ml \
     builtins_php.ml \
     controlflow_php.ml  controlflow_build_php.ml \
     callgraph_php.ml \
     database_php.ml \
     graph_php.ml \

#note: tools/phpmanual_xml.ml is related to builtins_php.ml

#     pil.ml pretty_print_pil.ml controlflow_pil.ml \
#     meta_pil.ml visitor_pil.ml \
#     pil_build.ml controlflow_build_pil.ml \
#cfa_pil.ml

#    dataflow_php.ml \
#     dataflow_pil.ml \

#aliasing_function_php.ml
#     bottomup_analysis_php.ml \

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/commons.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/commons/lib-json $(TOP)/commons/lib-xml \
   $(TOP)/external/ocamlpcre/lib \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   ../../parsing \
   .. \
   ../basic \

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


