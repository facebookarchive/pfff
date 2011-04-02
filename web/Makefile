TOP=..
OCSIGENDIR=ocsigen
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################
PROJECTNAME := graffiti

SERVERFILES= \
  htmlize_php2.ml global_db.ml lxr.ml \
  shared.ml \
  graffiti_server.ml \
  ${PROJECTNAME}.eliom \
  server.ml

CLIENTFILES= shared.ml \
  graffiti_client.ml \
  ${PROJECTNAME}.eliom

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/h_program-lang/ \
 $(TOP)/lang_php/parsing \
 $(TOP)/lang_php/analyze/foundation \
 $(TOP)/lang_php/analyze \

INCLUDES?=$(INCLUDEDIRS:%=-I %) $(SYSINCLUDES)

SERVERLIB := $(INCLUDES) -package ocsigen.deriving.syntax -package cairo
CLIENTLIB := -package ocsigen.deriving.syntax -package oclosure -package js_of_ocaml.syntax

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLFIND      := ocamlfind
OCAML          := ocaml
JS_OF_OCAML    := js_of_ocaml

ELIOMCLIENTDIR := $(shell ${OCAMLFIND} query ocsigen.ext.eliom_client)
OCLOSUREDIR    := $(shell ${OCAMLFIND} query oclosure)

TEMP           := _build

STATICDIR      := /tmp/static

########################################################################

##############################################################################
# Top rules
##############################################################################

.PHONY: all byte opt client server-byte server-opt

all: server-byte server-opt client
byte: server-byte client
opt: server-opt client

client: $(STATICDIR)/$(PROJECTNAME).js $(STATICDIR)/$(PROJECTNAME)_req.js
server-byte: $(PROJECTNAME).cma
server-opt: $(PROJECTNAME).cmxs

clean::
	-rm -rf ${TEMP} \
	   ${PROJECTNAME}.cma  \
	   ${PROJECTNAME}.a  ${PROJECTNAME}.cmxa ${PROJECTNAME}.cmxs \
	   $(STATICDIR)/${PROJECTNAME}.js $(STATICDIR)/${PROJECTNAME}_req.js

##############################################################################
# Generic ocaml rules
########################################################################
include Makefile.eliom

##############################################################################
# Test
########################################################################

run:
	rm -f $(STATICDIR)/css/*
	cp -a css/ $(STATICDIR)
	cp -a images/ $(STATICDIR)
	cp -a $(OCSIGENDIR)/files/mime.types $(STATICDIR)
	LD_LIBRARY_PATH=../external/ocamlbdb $(OCSIGENDIR)/server/ocsigen -c config/ocsigen.conf

clean_data::
	-rm -f var/logs/*.log var/logs/ocsigen_command

distclean:
	-rm -f *~ .\#* \#*\#
