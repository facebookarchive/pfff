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
	eliomc -c $(INCLUDES) global_db.ml
	eliomc -c $(INCLUDES) -package tyxml htmlize_php2.ml
	eliomc -c $(INCLUDES) lxr_server.ml
	eliomc -c $(INCLUDES) server.ml
	eliomc  -a -o var/app.cma \
         _server/global_db.cmo _server/htmlize_php2.cmo _server/lxr_server.cmo\
         _server/dump_server.cmo _server/server.cmo

run:
	CAML_LD_LIBRARY_PATH=../external/ocamlbdb ocsigenserver -c ocsigen.conf

##############################################################################
# Generic ocaml rules
##############################################################################
