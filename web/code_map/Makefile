TOP=../..
#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/web/commons \
  $(TOP)/h_visualization/ \
  $(TOP)/h_program-visual/ \

OBJS_SERVER= \
 flag_visual.cmo \
 model_codemap.cmo \
 server_codemap.cmo \

OBJS_CLIENT= \
 flag_visual.cmo \
 model_codemap.cmo \
 draw_microlevel.cmo \
 draw_macrolevel.cmo \
 draw_labels.cmo \
 client_codemap.cmo \

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

all:: lib_client.cma lib.cma 

# server-side
lib.cma: $(OBJS_SERVER)
	$(OCAMLC) -a -o $@ $^ 

# client-side
lib_client.cma: $(OBJS_CLIENT)
	$(OCAMLC) -a -o $@ $^ 
