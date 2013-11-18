TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= graph_code.ml \
     graph_code_opti.ml dependencies_matrix_code.ml \
     graph_code_tags.ml  graph_code_database.ml graph_code_prolog.ml \
     graph_code_analysis.ml graph_code_class_analysis.ml \
     layer_graph_code.ml \
     test_graph_code.ml unit_graph_code.ml

SYSLIBS= str.cma unix.cma
LIBS=../commons/lib.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/commons/lib-json \
 $(TOP)/commons/ocamlextra \
 $(TOP)/h_program-lang \
 $(TOP)/external/ocamlgraph $(TOP)/external/ocamlgraph/src \

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

##############################################################################
# install
##############################################################################
LIBNAME=pfff-graph_code
EXPORTSRC=

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) $(EXPORTSRC:%.mli=%.cmt)
