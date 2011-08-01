TOP=..
#############################################################################
# Configuration section
#############################################################################

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/h_program-lang/ \
 $(TOP)/lang_php/parsing \
 $(TOP)/lang_php/analyze/foundation \
 $(TOP)/lang_php/analyze \

INCLUDES?=$(INCLUDEDIRS:%=-I %) $(SYSINCLUDES)

FLAGS=-dtypes -g

LIBS_SHARED= -package deriving-ocsigen.syntax,js_of_ocaml.deriving.syntax \
  -syntax camlp4o
LIBS_SERVER= -thread -package tyxml,eliom,eliom.server \
  $(LIBS_SHARED)
LIBS_CLIENT= -package js_of_ocaml,eliom,eliom.client,oclosure \
  -package eliom.syntax,js_of_ocaml.syntax -syntax camlp4o \
  $(LIBS_SHARED)

##############################################################################
# Variables
##############################################################################

##############################################################################
# Top rules
##############################################################################

all: var/app.cma var/static/app_oclosure.js

# server-side
var/app.cma: _server/global_db.cmo _server/htmlize_php2.cmo _server/lxr_server.cmo _server/dump_server.cmo _server/server.cmo
	eliomc  -a -o $@ $^

_server/global_db.cmo: global_db.ml
	eliomc -c $(INCLUDES) $^

_server/htmlize_php2.cmo: htmlize_php2.ml
	eliomc -c $(INCLUDES) -package tyxml $^

_server/lxr_server.cmo: lxr_server.ml
	eliomc -c $(INCLUDES) $^

_server/dump_server.cmo: dump_server.ml
	eliomc -c $(INCLUDES) $^

_server/server.cmo: server.ml
	eliomc -c $(INCLUDES) $^

# client-side
var/static/app_oclosure.js: var/static/app.js
	oclosure_req $^

var/static/app.js: _client/shared.cmo
	js_of_eliom -o $@ -package oclosure  $^

_client/shared.cmo: shared.ml
	js_of_eliom -c $^

run:
	CAML_LD_LIBRARY_PATH=../external/ocamlbdb ocsigenserver -c ocsigen.conf

annot:
	ocamlfind ocamlc -dtypes -c $(LIBS_SHARED) shared.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) global_db.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) htmlize_php2.mli
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) htmlize_php2.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) lxr_server.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) dump_server.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) server.ml
	rm -f *.cm*

##############################################################################
# Generic ocaml rules
##############################################################################
