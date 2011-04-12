TOP=..
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

SRC_SERVER=lib.ml global_db.ml htmlize_php2.ml

SRC_CLIENT+=codemap_shared.ml codemap_client.ml app_codemap_client.ml
SRC_SERVER+=codemap_shared.ml codemap_server.ml app_codemap_server.ml

SRC_SERVER+=lxr_server.ml app_lxr_server.ml
SRC_SERVER+=dump_server.ml

SRC_SERVER+=server.ml

LIBS_SERVER+=-package cairo -package ocsigen.ext.eliom 
LIBS_CLIENT+=-package js_of_ocaml -package oclosure \
 -package ocsigen.ext.eliom_client

INCLUDES_SERVER=-package cairo -package ocsigen.ext.eliom \
  -package ocsigen.deriving.syntax -syntax camlp4o 
INCLUDES_CLIENT=-package js_of_ocaml -package oclosure -package ocsigen.ext.eliom_client \
  -package js_of_ocaml.syntax -syntax camlp4o

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/h_program-lang/ \
 $(TOP)/lang_php/parsing \
 $(TOP)/lang_php/analyze/foundation \
 $(TOP)/lang_php/analyze \

INCLUDES?=$(INCLUDEDIRS:%=-I %) $(SYSINCLUDES)
INCLUDES+=-package ocsigen.ext.eliom

INCLUDES_SHARED=-package ocsigen.deriving.syntax -syntax camlp4o

FLAGS=-dtypes -g

##############################################################################
# Top rules
##############################################################################

all: server.cma client.js

server.cma: $(SRC_SERVER:.ml=.cmo)
	ocamlfind ocamlc -a -o $@ -thread $(LIBS_SERVER) $^

ELIOMJS=$(shell ocamlfind query ocsigen.ext.eliom_client)/eliom_client.js
REQUIREMENTSML=$(shell ocamlfind query oclosure)/requirements.ml

client.js: $(SRC_CLIENT:.ml=.cmo)
	ocamlfind ocamlc $(LIBS_CLIENT) -linkpkg $^ -o client
	js_of_ocaml -pretty . $(ELIOMJS) client -o $@
	js_of_ocaml -pretty /tmp/static $(ELIOMJS) client -o /tmp/static/client.js
	ocaml str.cma $(REQUIREMENTSML) /tmp/static/client.js

clean::
	rm -f client client.js

codemap_shared.cmo: codemap_shared.ml
	ocamlfind ocamlc -thread -c $(FLAGS) $(INCLUDES_SHARED) $^
codemap_server.cmo: codemap_server.ml
	ocamlfind ocamlc -thread -c $(FLAGS) $(INCLUDES_SERVER) $^
codemap_client.cmo: codemap_client.ml
	ocamlfind ocamlc -thread -c $(FLAGS) $(INCLUDES_CLIENT) $^

app_codemap_server.cmo: app_codemap_server.ml
	ocamlfind ocamlc -thread -c $(FLAGS) $(INCLUDES_SERVER) $^
app_codemap_client.cmo: app_codemap_client.ml
	ocamlfind ocamlc -thread -c $(FLAGS) $(INCLUDES_CLIENT) $^

ELIOMSYNTAX=$(shell ocamlfind query ocsigen)/syntax
JSSYNTAX=$(shell ocamlfind query js_of_ocaml)

# .eliom file expansion
app_codemap.type.mli: app_codemap.eliom codemap_shared.cmo codemap_server.cmo codemap_client.cmo
	ocamlfind ocamlc -thread -syntax camlp4o -ppopt -impl -package ocsigen.ext.eliom,ocsigen.ext.eliom.syntax.type -i -impl app_codemap.eliom > $@
app_codemap_server.ml: app_codemap.eliom app_codemap.type.mli
	camlp4o $(ELIOMSYNTAX)/pa_eliom_seed.cmo $(ELIOMSYNTAX)/pa_eliom_client_server.cmo $(JSSYNTAX)/pa_js.cmo -type app_codemap.type.mli  pr_o.cmo  -impl app_codemap.eliom -o $@
app_codemap_client.ml: app_codemap.eliom app_codemap.type.mli
	camlp4o $(ELIOMSYNTAX)/pa_eliom_seed.cmo $(ELIOMSYNTAX)/pa_eliom_client_client.cmo $(JSSYNTAX)/pa_js.cmo -type app_codemap.type.mli  pr_o.cmo  -impl app_codemap.eliom -o $@

beforedepend:: app_codemap.type.mli app_codemap_server.ml app_codemap_client.ml

app_lxr_server.ml: app_lxr.eliom
	cp app_lxr.eliom $@

beforedepend:: app_lxr_server.ml


STATICDIR=/tmp/static
OCSIGENDIR=ocsigen
run: 
	rm -f $(STATICDIR)/css/*
	cp -a css/ $(STATICDIR)
	cp -a images/ $(STATICDIR)
	cp -a $(OCSIGENDIR)/files/mime.types $(STATICDIR)
	LD_LIBRARY_PATH=../external/ocamlbdb ocsigen -c ocsigen.conf

clean::
	rm -f var/logs/*.log var/logs/ocsigen_command

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	ocamlfind ocamlc -thread -c $(FLAGS) $(INCLUDES) $<
.mli.cmi:
	ocamlfind ocamlc -thread -c $(FLAGS) $(INCLUDES) $<
.ml.cmx:
	ocamlfind ocamlopt -thread -c $(FLAGS) $(INCLUDES) $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ *.exe gmon.out #*#

distclean::
	rm -f .depend

beforedepend::

depend:: beforedepend
	ocamlfind ocamldep $(INCLUDES_SHARED) $(INCLUDES_CLIENT)  $(SRC_CLIENT) > .depend
	ocamlfind ocamldep $(INCLUDES_SHARED) $(INCLUDES_SERVER) $(SRC_SERVER) >> .depend

-include .depend
