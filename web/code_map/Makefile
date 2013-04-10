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

# $(TOP)/h_program-lang/ \
# $(TOP)/lang_php/parsing \
#  $(TOP)/lang_php/analyze \
#  $(TOP)/lang_php/analyze/foundation \
#  $(TOP)/web/code_map \
#  $(TOP)/web/code_graph \
#  $(TOP)/web/lxr \

OBJS_SERVER= \
 shared_codemap.cmo \
 server_codemap.cmo \

OBJS_CLIENT= \
 draw_macro.cmo \
 client_codemap.cmo \

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
