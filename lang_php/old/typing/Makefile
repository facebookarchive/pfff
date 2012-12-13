TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= \
     typing_php.ml \
     type_annotater_php.ml \
     typing_weak_php.ml \
     typing_trivial_php.ml \
     type_inference_pil.ml \

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/commons/lib-json \
   $(TOP)/external/ocamlpcre/lib \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   ../../parsing \
   .. \
   ../basic \
   ../foundation \

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


