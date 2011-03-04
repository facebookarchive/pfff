#############################################################################
# Configuration section
#############################################################################

include ../../Makefile.config

##############################################################################
# Variables
##############################################################################

SRC=hello.ml

# when ocsigen is installed, can use package name instead of those -I
LIBSOCSIGEN=$(LIBDIRS2)
LIBS = -package lwt.unix,pcre $(LIBSOCSIGEN)

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLC= $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIBS)
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

byte: $(OBJS)

opt: $(OBJSOPT)
	$(CMXSCMD)

run:
	../../server/ocsigen -c files/hello.conf

clean::
	-rm -f files/*.log files/ocsigen_command

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
