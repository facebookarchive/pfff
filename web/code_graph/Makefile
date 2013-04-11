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
  $(TOP)/h_visualization/ \
  $(TOP)/h_program-visual/ \

OBJS_SERVER= \
 shared_codegraph.cmo \
 server_codegraph.cmo \

OBJS_CLIENT= \
 view_matrix_codegraph.cmo \
 client_codegraph.cmo \

LIBS_CLIENT= -package js_of_ocaml.syntax -syntax camlp4o

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
