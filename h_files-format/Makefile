TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= outline.ml simple_format.ml source_tree.ml

LIBS=$(TOP)/commons/lib.cma
INCLUDEDIRS= $(TOP)/commons

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa
opt:: all.opt


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

##############################################################################
# install
##############################################################################
LIBNAME=pfff-h_files-format
EXPORTSRC=\
  outline.mli

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
