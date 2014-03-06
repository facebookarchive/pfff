TOP=..
##############################################################################
# Variables
##############################################################################
-include $(TOP)/Makefile.config

TARGET=lib

SRC= flag_program_visual.ml \
     treemap_pl.ml \
     layer_archi.ml \
     layer_vcs.ml \
     test_program_visual.ml


LIBS=../commons/lib.cma \
   ../h_visualization/lib.cma \
   ../h_version-control/lib.cma \
   ../h_program-lang/lib.cma \

INCLUDEDIRS= ../commons \
   ../h_visualization \
   ../h_version-control \
   ../h_program-lang \


#nomore: graphics.cma, create conflict when link with lablgtk
SYSLIBS=nums.cma bigarray.cma str.cma unix.cma 

ifeq ($(FEATURE_GRAPHICS),1)
SRC2+=graphics/treemap_pl_graphics.ml
endif

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(PROGS)
all.opt:: $(TARGET).cmxa  $(PROGS:=.opt)

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)
