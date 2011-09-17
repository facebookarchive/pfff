TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

# mostly local analysis
SRC= \
  error_php.ml \
  env_check_php.ml check_variables_helpers_php.ml check_variables_php.ml \
  check_cfg_php.ml \
  check_dfg_php.ml \
#  unit_checker_php.ml \
#  check_includes_php.ml \

# mostly global analysis
SRC+= \
  check_functions_php.ml \
  check_classes_php.ml \

SRC+= check_all_php.ml layer_checker_php.ml

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/commons.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons/ocollection $(TOP)/commons/ocamlextra \
   $(TOP)/commons/lib-json \
   $(TOP)/external/ocamlpcre/lib \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   ../../parsing \
   .. \
   ../basic \
   ../foundation \

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


