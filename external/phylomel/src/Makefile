TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= extArray.ml extList.ml extString.ml std.ml IO.ml \
     distMat.ml genotype.ml genotypes.ml \
     genoMat.ml dynMat.ml \
     svg.ml \
     dendogram.ml \
     vec2.ml \
     tree.ml \
     phylogram.ml \
     clustering.ml \
     barnesHut.ml forceDirectedLayout.ml \

LIBS=
#../commons/lib.cma
INCLUDEDIRS=$(TOP)/commons/ocamlextra

SYSLIBS= 
#nums.cma bigarray.cma str.cma unix.cma 

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
