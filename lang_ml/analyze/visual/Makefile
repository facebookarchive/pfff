TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= highlight_ml.ml \


SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/h_program-lang  \
   $(TOP)/lang_ml/parsing \


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
# install
##############################################################################
LIBNAME=pfff-lang_ml-visual
EXPORTSRC=\
  highlight_ml.mli

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
