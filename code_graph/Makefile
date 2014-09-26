TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=cairo_helpers3.ml \
    model3.ml controller3.ml \
    view_matrix.ml view_overlays3.ml \
    view3.ml

SYSLIBS=str.cma unix.cma dbm.cma threads.cma  lablgtk.cma

LIBS=$(TOP)/commons/lib.cma $(TOP)/commons/commons_gui.cma \
     $(TOP)/commons/commons_features.cma


INCLUDEDIRS=$(TOP)/commons $(TOP)/globals \
	$(TOP)/h_program-lang \
	$(TOP)/graph_code \
	$(TOP)/h_visualization \
	$(TOP)/h_program-visual \

SYSINCLUDES=\
  -I $(TOP)/external/ocamlgtk/src \
  -I $(TOP)/external/ocamlcairo/src \

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma 
all.opt:: $(TARGET).cmxa
opt: all.opt


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

clean::
	rm -f $(TARGET).top

EXTRA_FLAGS=-w -3
cairo_helpers3.cmo: cairo_helpers3.ml
	$(OCAMLC) $(EXTRA_FLAGS) -c $<

cairo_helpers3.cmx: cairo_helpers3.ml
	$(OCAMLOPT) $(PP) $(EXTRA_FLAGS) -c $<

##############################################################################
# Generic rules
##############################################################################
