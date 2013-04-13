TOP=../..
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/h_program-lang/ \
 $(TOP)/h_visualization/ \

OBJS_SERVER= \
 flag_web.cmo \
 common_server.cmo \

OBJS_CLIENT= \
 common_client.cmo \
 canvas_helpers.cmo \

LIBS_CLIENT= -package js_of_ocaml.syntax -syntax camlp4o

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

OCAMLC=ocamlfind ocamlc -annot $(INCLUDES) $(LIBS_CLIENT)
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
