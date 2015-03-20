TOP=..
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/jsonwheel \
 $(TOP)/h_program-lang/ \
 $(TOP)/graph_code/ \
 $(TOP)/h_visualization/ \
 $(TOP)/h_program-visual/ \
 $(TOP)/lang_php/parsing \
 $(TOP)/web/commons \
  $(TOP)/web/code_map \
  $(TOP)/web/code_graph \
  $(TOP)/web/lxr \

INCLUDES?=$(INCLUDEDIRS:%=-I %) $(SYSINCLUDES)

MAKESUBDIRS=commons code_map code_graph
#lxr

# for 'make annot'
LIBS_SHARED= -package deriving-ocsigen.syntax,js_of_ocaml.deriving.syntax \
  -syntax camlp4o
LIBS_SERVER= -thread -package tyxml,eliom,eliom.server \
  $(LIBS_SHARED)
LIBS_CLIENT= -package js_of_ocaml,eliom,eliom.client \
  -package eliom.syntax,js_of_ocaml.syntax -syntax camlp4o \
  $(LIBS_SHARED)

OBJS_SERVER= \
 _server/globals.cmo \
 _server/dump_server.cmo \
 _server/blueprint_server.cmo \
 _server/codemap.cmo \
 _server/codegraph.cmo \
 _server/test.cmo \
 _server/home.cmo \

OBJS_CLIENT= \
 _client/codemap.cmo \
 _client/codegraph.cmo \
 _client/test.cmo \
 _client/home.cmo \

# We need h_program-lang for Database_code, but this leads to
# requiring in turn ocamlgraph :( hopefully it will not be too big for
# js_of_ocaml and we don't send a too big js file on the wire
LIBS_CLIENT2= $(TOP)/commons/lib.cma \
  $(TOP)/external/jsonwheel/lib.cma \
  $(TOP)/external/ocamlgraph/ocamlgraph.cma \
  $(TOP)/commons/graph/lib.cma \
  $(TOP)/h_program-lang/lib.cma \
  $(TOP)/graph_code/lib.cma \
  $(TOP)/h_visualization/lib.cma \
  $(TOP)/web/commons/lib_client.cma \
  $(TOP)/web/code_map/lib_client.cma \
  $(TOP)/web/code_graph/lib_client.cma \

SYSLIBS_CLIENT=str.cma unix.cma

# see ocsigen.conf eliom module section
LIBS_SERVER2=
SYSLIBS_SERVER=

#to debug, but not that good, not enough
JSOPT=-jsopt -pretty -jsopt -noinline

ELIOMC=eliomc -g -annot
JS_OF_ELIOM=js_of_eliom -g -annot

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
var/app.cma: $(LIBS_SERVER2) $(OBJS_SERVER)
	eliomc -g -a -o $@ $(SYSLIBS_SERVER) $^

_server/home.cmo: home.eliom
	$(ELIOMC)  -c $(INCLUDES) $^
_server/codemap.cmo: codemap.eliom
	$(ELIOMC)  -c $(INCLUDES) $^
_server/codegraph.cmo: codegraph.eliom
	$(ELIOMC)  -c $(INCLUDES) $^
_server/test.cmo: test.eliom
	$(ELIOMC)  -c $(INCLUDES) $^


# client-side
var/static/app.js: $(OBJS_CLIENT) $(LIBS_CLIENT2) runtime2.js
	$(JS_OF_ELIOM) -I ../commons $(JSOPT) -o $@ $(SYSLIBS_CLIENT) $(LIBS_CLIENT2)  $(OBJS_CLIENT) -jsopt runtime2.js


_client/home.cmo: home.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
	$(JS_OF_ELIOM) $(JSOPT) -c $^
_client/codemap.cmo: codemap.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
	$(JS_OF_ELIOM) $(JSOPT) -c $(INCLUDES) $^
_client/codegraph.cmo: codegraph.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
	$(JS_OF_ELIOM) $(JSOPT) -c $(INCLUDES) $^
_client/test.cmo: test.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
	$(JS_OF_ELIOM) $(JSOPT) -c $(INCLUDES) $^

# you can add -V, this helps sometimes
OCFLAGS=

run: all
	LD_LIBRARY_PATH=../commons ocsigenserver $(OCFLAGS) -c ocsigen.conf

rerun:
	pkill ocsigenserver
	sleep 1
	make run

annot:
	rm -f *.annot
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) dump_server.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) blueprint_server.ml
	ocamlfind ocamlc -dtypes -c $(INCLUDES) $(LIBS_SERVER) globals.ml
	$(ELIOMC) -annot -c $(INCLUDES) test.eliom
	ln -s _server/test.annot
	$(ELIOMC) -annot -c $(INCLUDES) codemap.eliom
	ln -s _server/codemap.annot
	$(ELIOMC) -annot -c $(INCLUDES) codegraph.eliom
	ln -s _server/codegraph.annot
	$(ELIOMC) -annot -c $(INCLUDES) home.eliom
	ln -s _server/home.annot
	rm -f *.cm*

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done

depend::
	set -e; for i in $(MAKESUBDIRS); do echo $$i; $(MAKE) -C $$i depend; done

clean::
	rm -rf _client _server *.cm*

depend::
	eliomdep -client *.ml *.eliom $(INCLUDES)  > .depend
	eliomdep -server *.ml *.eliom $(INCLUDES)  >> .depend

# additional dependencies needed, not sure why
_server/test.type_mli: test.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
_server/codemap.type_mli: codemap.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^
_server/codegraph.type_mli: codegraph.eliom
	$(ELIOMC)  -infer $(INCLUDES) $^

# eliomdep does not handle dependencies between .eliom files for now
_server/home.cmo: _server/codemap.cmo
_server/home.cmo: _server/codegraph.cmo
_server/home.cmo: _server/test.cmo
_server/codemap.type_mli: _server/globals.cmo

_client/codegraph.cmo: code_graph/lib_client.cma
_server/codegraph.cmo: _server/globals.cmo
_client/codemap.cmo: code_map/lib_client.cma
_server/codemap.cmo: _server/globals.cmo
_client/test.cmo: code_map/lib_client.cma

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
