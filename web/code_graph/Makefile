TOP=../..
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/web/commons \
  $(TOP)/h_program-lang/ \
  $(TOP)/graph_code/ \
  $(TOP)/h_visualization/ \
  $(TOP)/h_program-visual/ \

OBJS_SHARED= \
 model_codegraph.cmo \

OBJS_SERVER= \
 $(OBJS_SHARED) \
 server_codegraph.cmo \

OBJS_CLIENT= \
 $(OBJS_SHARED) \
 view_overlays_codegraph.cmo \
 view_matrix_codegraph.cmo \
 interaction_codegraph.cmo \
 client_codegraph.cmo \

LIBS_CLIENT= \
  -package eliom.client \
  -package lwt.syntax -package js_of_ocaml.syntax -syntax camlp4o

LIBS_SERVER= \
  -package eliom.server \

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

OCAMLC=ocamlfind ocamlc -g -annot $(INCLUDES) $(LIBS_CLIENT)
OCAMLDEP=ocamlfind ocamldep $(INCLUDES) $(LIBS_CLIENT)

##############################################################################
# Top rules
##############################################################################

all:: lib.cma lib_client.cma

# server-side
lib.cma: $(OBJS_SERVER)
	$(OCAMLC) -a -o $@ $^ 

# client-side
lib_client.cma: $(OBJS_CLIENT)
	$(OCAMLC) -a -o $@ $^ 
