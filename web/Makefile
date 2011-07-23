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

##############################################################################
# Variables
##############################################################################

##############################################################################
# Top rules
##############################################################################

all:
	eliomc -c $(INCLUDES) dump_server.ml
	eliomc -c server.ml
	eliomc  -a -o var/app.cma _server/dump_server.cmo _server/server.cmo

run:
	CAML_LD_LIBRARY_PATH=../external/ocamlbdb ocsigenserver -c ocsigen.conf

##############################################################################
# Generic ocaml rules
##############################################################################
