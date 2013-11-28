TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= flag_matcher.ml \
     parse_fuzzy.ml matching_report.ml lib_unparser.ml \
     metavars_fuzzy.ml fuzzy_vs_fuzzy.ml \
     matching_fuzzy.ml transforming_fuzzy.ml \
     sgrep_fuzzy.ml spatch_fuzzy.ml \
     unit_matcher.ml

#todo: refactoring_code_fuzzy.ml

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma 

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons/ocollection \
   $(TOP)/commons/ocamlextra \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/external/ \

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
LIBNAME=pfff-matcher
EXPORTSRC=

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \

