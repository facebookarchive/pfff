TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= errors_clang.ml \
     location_clang.ml \
     type_clang.ml \
     uninclude_clang.ml \
     compile_commands_clang.ml \
     graph_code_clang.ml \
     test_analyze_clang.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     $(TOP)/h_version-control/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/external/jsonwheel \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/graph_code \
   $(TOP)/globals \
   $(TOP)/lang_cpp/parsing \
   ../parsing \

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
# Literate Programming rules
##############################################################################
