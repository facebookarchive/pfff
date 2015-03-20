TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= graph.ml
SYSLIBS= str.cma unix.cma
LIBS=../../external/ocamlgraph/ocamlgraph.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/external/ocamlgraph/ \
 $(TOP)/external/ocamlgraph/src \

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

##############################################################################
# install
##############################################################################
LIBNAME=pfff-commons-graph
EXPORTSRC=\
  graph.mli

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi)
