TOP=..
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/h_program-lang/ \
 $(TOP)/lang_php/parsing \
  $(TOP)/lang_php/analyze \
  $(TOP)/lang_php/analyze/foundation \

INCLUDES?=$(INCLUDEDIRS:%=-I %) $(SYSINCLUDES)

FLAGS=-dtypes -g

LIBS_SHARED= -package deriving-ocsigen.syntax,js_of_ocaml.deriving.syntax \
  -syntax camlp4o
LIBS_SERVER= -thread -package tyxml,eliom,eliom.server \
  $(LIBS_SHARED)
LIBS_CLIENT= -package js_of_ocaml,eliom,eliom.client,oclosure \
  -package eliom.syntax,js_of_ocaml.syntax -syntax camlp4o \
  $(LIBS_SHARED)

OBJS_SERVER= \
 _server/global_db.cmo _server/htmlize_php2.cmo \
 _server/lxr_server.cmo _server/dump_server.cmo \
 _server/server.cmo \
 _server/codemap.cmo \
 _server/home.cmo \

OBJS_CLIENT= \
 _client/shared.cmo \
 _client/client.cmo \
 _client/codemap.cmo \
 _client/home.cmo \

##############################################################################
# Top rules
##############################################################################

all: var/app.cma var/static/app_oclosure.js

# server-side
var/app.cma: $(OBJS_SERVER)
	eliomc  -a -o $@ $^

_server/global_db.cmo: global_db.ml
	eliomc -c $(INCLUDES) $^

_server/htmlize_php2.cmo: htmlize_php2.ml
	eliomc -c $(INCLUDES) -package tyxml htmlize_php2.mli
	eliomc -c $(INCLUDES) -package tyxml $^

_server/lxr_server.cmo: lxr_server.ml
	eliomc -c $(INCLUDES) $^

_server/dump_server.cmo: dump_server.ml
	eliomc -c $(INCLUDES) $^

_server/server.cmo: server.ml
	eliomc -c $(INCLUDES) $^

_server/home.cmo: home.eliom
	eliomc  -c -noinfer $(INCLUDES) $^

_server/codemap.cmo: codemap.eliom
	eliomc  -c -noinfer $(INCLUDES) $^


# client-side
var/static/app_oclosure.js: var/static/app.js
	oclosure_req $^

var/static/app.js: $(OBJS_CLIENT)
	js_of_eliom -o $@ -package oclosure  $^

_client/shared.cmo: shared.ml
	js_of_eliom -c $^

_client/client.cmo: client.ml
	js_of_eliom -c -package oclosure $^

_client/home.cmo: home.eliom
	eliomc  -infer $(INCLUDES) $^
	js_of_eliom -c $^

_client/codemap.cmo: codemap.eliom
	eliomc  -infer $(INCLUDES) $^
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

clean::
	rm -rf _client _server

##############################################################################
# Generic ocaml rules
##############################################################################
