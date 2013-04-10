TOP=..
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/h_program-lang/ \
 $(TOP)/h_visualization/ \
 $(TOP)/h_program-visual/ \
 $(TOP)/lang_php/parsing \
  $(TOP)/lang_php/analyze \
  $(TOP)/lang_php/analyze/foundation \
  $(TOP)/web/commons \
  $(TOP)/web/code_map \
  $(TOP)/web/code_graph \
  $(TOP)/web/lxr \

INCLUDES?=$(INCLUDEDIRS:%=-I %) $(SYSINCLUDES)

MAKESUBDIRS=commons code_map

# for 'make annot'
LIBS_SHARED= -package deriving-ocsigen.syntax,js_of_ocaml.deriving.syntax \
  -syntax camlp4o
LIBS_SERVER= -thread -package tyxml,eliom,eliom.server \
  $(LIBS_SHARED)
LIBS_CLIENT= -package js_of_ocaml,eliom,eliom.client \
  -package eliom.syntax,js_of_ocaml.syntax -syntax camlp4o \
  $(LIBS_SHARED)

OBJS_SERVER= \
 _server/dump_server.cmo \
 _server/codemap.cmo \
 _server/test.cmo \
 _server/home.cmo \

# _server/global_db.cmo _server/htmlize_php2.cmo \
# _server/lxr_server.cmo \

OBJS_CLIENT= \
 _client/codemap.cmo \
 _client/test.cmo \
 _client/home.cmo \

LIBS_CLIENT2= $(TOP)/commons/lib.cma \
  $(TOP)/h_visualization/lib.cma \
  $(TOP)/web/commons/lib_client.cma \
  $(TOP)/web/code_map/lib_client.cma \


SYSLIBS_CLIENT=str.cma unix.cma

LIBS_SERVER2=
SYSLIBS_SERVER=

#to debug
JSOPT=-jsopt -pretty -jsopt -noinline

ELIOMC=eliomc -annot
JS_OF_ELIOM=js_of_eliom -annot

##############################################################################
# Top rules
##############################################################################

#note: need compile client side before server side because of the -infer
# done when compile client side modules, which is needed anyway for
# compiling the server side
all: rec var/static/app.js var/app.cma

#	$(MAKE) rec 
#	$(MAKE) var/static/app.js 
#	$(MAKE) var/app.cma 

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

# server-side
var/app.cma: $(OBJS_SERVER)
	eliomc -a -o $@ $(SYSLIBS_SERVER) $(LIBS_SERVER2) $^

_server/htmlize_php2.cmo: htmlize_php2.ml
	$(ELIOMC) -c $(INCLUDES) -package tyxml htmlize_php2.mli
	$(ELIOMC) -c $(INCLUDES) -package tyxml $^


_server/home.cmo: home.eliom
	$(ELIOMC)  -c $(INCLUDES) $^
_server/codemap.cmo: codemap.eliom
	$(ELIOMC)  -c $(INCLUDES) $^
_server/test.cmo: test.eliom
	$(ELIOMC)  -c $(INCLUDES) $^


# client-side
var/static/app.js: $(OBJS_CLIENT) runtime1.js
	$(JS_OF_ELIOM) $(JSOPT) -o $@ $(SYSLIBS_CLIENT) $(LIBS_CLIENT2)  $(OBJS_CLIENT) -jsopt runtime1.js


_client/home.cmo: home.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
	$(JS_OF_ELIOM) $(JSOPT) -c $^
_client/codemap.cmo: codemap.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
	$(JS_OF_ELIOM) $(JSOPT) -c $(INCLUDES) $^
_client/test.cmo: test.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
	$(JS_OF_ELIOM) $(JSOPT) -c $(INCLUDES) $^

# additional dependencies needed, not sure why
_server/test.type_mli: test.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
_server/codemap.type_mli: codemap.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^

run:
	ocsigenserver -c ocsigen.conf

annot:
	ocamlfind ocamlc -dtypes -c $(LIBS_SHARED) shared.ml
	ocamlfind ocamlc -dtypes -c $(LIBS_SHARED) flag_web.ml
#	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) global_db.ml
#	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) htmlize_php2.mli
#	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) htmlize_php2.ml
#	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) lxr_server.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) dump_server.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) server.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_CLIENT) common_client.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_CLIENT) canvas_helpers.mli
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_CLIENT) canvas_helpers.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_CLIENT) draw_macro.mli
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_CLIENT) draw_macro.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_CLIENT) client.ml
	$(ELIOMC) -annot -c $(INCLUDES) test.eliom
	ln -s _server/test.annot
	$(ELIOMC) -annot -c $(INCLUDES) codemap.eliom
	ln -s _server/codemap.annot
	$(ELIOMC) -annot -c $(INCLUDES) home.eliom
	ln -s _server/home.annot
	rm -f *.cm*

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

depend::
	set -e; for i in $(MAKESUBDIRS); do echo $$i; $(MAKE) -C $$i depend; done

clean::
	rm -rf _client _server

depend::
	eliomdep -client *.ml *.eliom  > .depend
	eliomdep -server *.ml *.eliom  >> .depend

# eliomdep does not handle dependencies between .eliom files for now
_server/home.cmo: _server/codemap.cmo
_server/home.cmo: _server/test.cmo

##############################################################################
# Generic ocaml rules
##############################################################################

_server/%.cmo: %.ml
	$(ELIOMC) -c $(INCLUDES) $^

_server/%.cmi: %.mli
	$(ELIOMC) -c $(INCLUDES) $^

_client/%.cmo: %.ml
	$(JS_OF_ELIOM) $(JSOPT) -c $(INCLUDES) $^

_client/%.cmi: %.mli
	$(JS_OF_ELIOM) $(JSOPT) -c $(INCLUDES) $^

clean::
	rm -f *.annot

-include .depend
