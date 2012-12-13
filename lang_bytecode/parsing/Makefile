TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= ast_bytecode.ml \
     lib_parsing_bytecode.ml \
     parse_bytecode.ml \
     test_parsing_bytecode.ml

# meta_ast_java.ml \
# unit_parsing_java.ml
# visitor_java.ml 

SYSLIBS= str.cma unix.cma 

LIBS= $(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
  $(TOP)/h_program-lang \
  $(TOP)/globals \
  $(TOP)/external/extlib \
  $(TOP)/external/javalib/src 

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
# Generic rules
##############################################################################
