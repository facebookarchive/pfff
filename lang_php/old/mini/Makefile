TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_mini_php.ml \
     ast_mini_php.ml \
     sexp_mini_php.ml \
     visitor_mini_php.ml \
     parse_mini_php.ml \
     typing_mini_php.ml \
     test_mini_php.ml


SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma \
  $(TOP)/h_program-lang/lib.cma \
  ../parsing/lib.cma

INCLUDEDIRS=$(TOP)/commons $(TOP)/commons/lib-sexp \
   $(TOP)/h_program-lang \
   ../parsing

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

visitor_mini_php.cmo: visitor_mini_php.ml
	$(OCAMLC) -w y -c $<

##############################################################################
# Generic rules
##############################################################################
