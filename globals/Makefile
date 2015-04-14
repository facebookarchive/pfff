TOP=..
-include $(TOP)/Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= config_pfff.ml

LIBS=
INCLUDEDIRS=../commons

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma

all.opt: $(TARGET).cmxa

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)


config_pfff.ml:
	@echo "config_pfff.ml is missing. Have you run ./configure?"
	@exit 1

distclean::
	rm -f config_pfff.ml

##############################################################################
# install
##############################################################################
LIBNAME=pfff-config
EXPORTSRC=\

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
