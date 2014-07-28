TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= ast_minic.ml ast_minijava.ml \
     meta_minic.ml \
     parse_minic.ml parse_minijava.ml \
     datalog_minic.ml datalog_minijava.ml \
     test_mini.ml unit_mini.ml \

SYSLIBS= str.cma unix.cma 

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \
 $(TOP)/lang_cpp/parsing/lib.cma \
 $(TOP)/lang_c/parsing/lib.cma

INCLUDEDIRS= \
 $(TOP)/commons $(TOP)/commons/ocamlextra \
 $(TOP)/globals \
 $(TOP)/h_program-lang \
 $(TOP)/lang_cpp/parsing \
 $(TOP)/lang_c/parsing \

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

##############################################################################
# Literate Programming rules
##############################################################################
