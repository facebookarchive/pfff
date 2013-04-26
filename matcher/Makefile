TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= lib_matcher.ml \
     lib_unparser.ml \
     metavars_fuzzy.ml \
     fuzzy_vs_fuzzy.ml \
     matching_fuzzy.ml \
     transforming_fuzzy.ml \
     sgrep_fuzzy.ml \
     spatch_fuzzy.ml \
     unit_matcher.ml

#flag_matcher_php.ml   \
#     refactoring_code_php.ml \

# put here also generic parts of:
#  token_views_of_xxx ? lib_parse_fuzzy


SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma 

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocollection \
   $(TOP)/commons/ocamlextra \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/external/ocamlpcre/lib \

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
