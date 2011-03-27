TOP=..
OCSIGENDIR=ocsigen
#############################################################################
# Configuration section
#############################################################################

include $(TOP)/Makefile.config
include $(OCSIGENDIR)/Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= \
 \
 models/product.ml \
 views/product/product_index.ml \
 views/product/product_form.ml \
 views/product/product_new.ml \
 views/product/product_edit.ml \
 views/product/product_show.ml \
 controllers/product_ctl.ml \
 \
 htmlize_php2.ml \
 app.ml \


INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/h_program-lang/ \
 $(TOP)/lang_php/parsing \
 $(TOP)/lang_php/analyze/foundation \
 $(TOP)/lang_php/analyze \


INCLUDES?=$(INCLUDEDIRS:%=-I %) $(SYSINCLUDES)

# when ocsigen is installed, can use package name instead of those -I
LIBSOCSIGEN=$(LIBDIRS2:../../%=$(OCSIGENDIR)/%)
LIBS = -package lwt.unix,pcre $(LIBSOCSIGEN)

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLC= $(OCAMLFIND) $(CAMLCNAME) -dtypes $(DBG) $(LIBS) $(INCLUDES)
OCAMLOPT= $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIBS)
OCAMLDEP= $(OCAMLFIND) ocamldep

ifeq "$(NATDYNLINK)" "YES"
CMXS=$(FILES:.ml=.cmxs)
CMXSCMD=cp $(OBJSOPT) ..
else
CMXS=
CMXSCMD=
endif

##############################################################################
# Generic variables
##############################################################################

OBJS=$(SRC:.ml=.cmo)
OBJSOPT=$(CMXS)

##############################################################################
# Top rules
##############################################################################

all:: $(TARGET).cma

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

byte: $(OBJS)

opt: $(OBJSOPT)
	$(CMXSCMD)

run:
	LD_LIBRARY_PATH=../external/ocamlbdb $(OCSIGENDIR)/server/ocsigen -c config/ocsigen.conf

clean_data::
	-rm -f var/logs/*.log var/logs/ocsigen_command

clean::
	find -name "*.cm[ioxa]" | xargs rm -f
	find -name "*.annot" | xargs rm -f

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs

.PHONY: doc

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.cmx.cmxs:
	$(OCAMLOPT) -shared -linkall -o $@ $<

clean::
	-rm -f *.cm[ioxa] *.cmxa *.cmxs *.o *~ $(NAME)

depend:
	$(OCAMLDEP) $(PP) $(LIBS) $(SRC:.ml=.mli) $(SRC) > .depend

-include .depend
