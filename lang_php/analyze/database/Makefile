TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

#note: database_php.ml is now in foundation/


SRC= \
     database_php_query.ml \
     database_php_statistics.ml \

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     $(TOP)/h_version-control/lib.cma \
     ../../parsing/lib.cma \
     ../foundation/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/commons/lib-json \
   $(TOP)/external/ocamlpcre/lib \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   ../../parsing \
   .. \
   ../foundation \

#   $(PCREINCLUDE) $(JANEINCLUDE)
#../matcher_php
#../parsing-comments \
#../smpl \

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


