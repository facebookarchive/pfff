TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= graph_code_cmt_helpers.ml graph_code_cmt.ml \
 unit_analyze_cmt.ml

OCAMLCOMPILERDIR=$(shell ocamlc -where)/compiler-libs

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocamlextra \
 $(OCAMLCOMPILERDIR) \
 $(TOP)/globals \
 $(TOP)/h_program-lang $(TOP)/graph_code \
 $(TOP)/lang_cmt/parsing \
 $(TOP)/lang_ml/parsing \


PP=-pp "cpp $(CLANG_HACK)  -DOCAML_VERSION=$(OCAMLVERSION)"

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



