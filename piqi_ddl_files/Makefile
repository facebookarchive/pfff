TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=\
    $(PIQI_ML_FILES) \
    $(PRINT_OPTIONS)

PIQI_FILES = graph_graphson.piqi
PIQI_ML_FILES = graph_graphson_piqi.ml graph_graphson_piqi_ext.ml \

		
PRINT_OPTIONS=graph_print_options.ml


SYSLIBS= str.cma unix.cma

LIBS= $(TOP)/external/piqi-ocaml/piqirun.cma

INCLUDEDIRS= $(TOP)/external/piqi-ocaml/piqirun \
	     $(TOP)/commons

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common
PIQIC = $(TOP)/external/piqi-ocaml/piqic-ocaml/piqic-ocaml
PIQIC_FLAGS = --multi-format

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i ; \
	done
graph_print_options.cma: $(TARGET).cma
clean::
	rm -f $(TARGET).top $(PIQI_ML_FILES)

