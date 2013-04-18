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
  $(TOP)/lang_ml/parsing \
   $(TOP)/lang_ml/analyze/visual \
  $(TOP)/lang_php/parsing \
   $(TOP)/lang_php/analyze \
   $(TOP)/lang_php/analyze/checker \
   $(TOP)/lang_php/analyze/visual \
   $(TOP)/lang_php/parsing \
   $(TOP)/lang_haskell/parsing \
    $(TOP)/lang_haskell/analyze \
   $(TOP)/lang_nw/parsing \
    $(TOP)/lang_nw/analyze \
   $(TOP)/lang_lisp/parsing \
    $(TOP)/lang_lisp/analyze \
   $(TOP)/lang_cpp/parsing \
    $(TOP)/lang_cpp/analyze \
   $(TOP)/lang_js/parsing \
    $(TOP)/lang_js/analyze \
   $(TOP)/lang_python/parsing \
    $(TOP)/lang_python/analyze \
   $(TOP)/lang_csharp/parsing \
    $(TOP)/lang_csharp/analyze \
   $(TOP)/lang_opa/parsing \
    $(TOP)/lang_opa/analyze \
   $(TOP)/lang_java/parsing \
    $(TOP)/lang_java/analyze \
   $(TOP)/lang_erlang/parsing \
    $(TOP)/lang_erlang/analyze \
   $(TOP)/lang_html/parsing \
    $(TOP)/lang_html/analyze \
   $(TOP)/lang_text \

OBJS_SERVER= \
 flag_visual.cmo \
 model_codemap.cmo \
 server_codemap_parsing.cmo \
 server_codemap.cmo \

OBJS_CLIENT= \
 flag_visual.cmo \
 style_codemap.cmo \
 model_codemap.cmo \
 draw_macrolevel.cmo \
 draw_microlevel.cmo \
 draw_labels.cmo \
 view_overlays_codemap.cmo \
 interaction_codemap.cmo \
 client_codemap.cmo \

LIBS_CLIENT= \
  -package eliom.client \
  -package lwt.syntax -package js_of_ocaml.syntax -syntax camlp4o

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
