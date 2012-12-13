TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=gui

SRC=model.ml controller.ml \
    view_globals.ml view_helpers.ml view_dialogs.ml \
    view_widgets.ml \
    view_widgets_entities.ml \
    source.ml \
    source_php.ml view_widgets_source.ml  \
    view_test.ml \
    view.ml \

#view_widgets_annotations.ml view_widgets_entities.ml

SYSLIBS=str.cma unix.cma dbm.cma threads.cma  lablgtk.cma lablgtksourceview.cma

LIBS=$(TOP)/commons/lib.cma $(TOP)/commons/commons_gui.cma \
     $(TOP)/commons/commons_features.cma \
     $(TOP)/globals/globals.cma \
     $(TOP)/parsing_php/parsing_php.cma 

INCLUDEDIRS=$(TOP)/commons $(TOP)/globals \
	$(TOP)/h_program-lang/ \
	$(TOP)/parsing_php \
        $(TOP)/analyze_php \
         $(TOP)/analyze_php/basic $(TOP)/analyze_php/foundation \
         $(TOP)/analyze_php/finder \
         $(TOP)/analyze_php/database \
        $(TOP)/matcher_php

SYSINCLUDES=-I ../external/ocamlgtk/src
# -I ../external/ocamlgl/src
#-I +lablgtk2  

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


##############################################################################
# Generic rules
##############################################################################
