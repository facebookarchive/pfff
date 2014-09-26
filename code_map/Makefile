TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=cairo_helpers.ml custom_list_generic.ml async.ml \
    flag_visual.ml style2.ml help.ml \
    parsing2.ml \
    editor_connection.ml \
    model_database_code.ml model_graph_code.ml \
    model2.ml controller2.ml \
    completion2.ml \
    draw_labels.ml draw_legend.ml \
    draw_macrolevel.ml draw_microlevel.ml  \
    view_mainmap.ml view_minimap.ml view_overlays.ml \
    ui_search.ml ui_navigation.ml ui_layers.ml \
    view2.ml

#experimental:  future.ml \    ancient2.ml \      completion.ml \

SYSLIBS=str.cma unix.cma dbm.cma threads.cma  lablgtk.cma

LIBS=$(TOP)/commons/lib.cma $(TOP)/commons/commons_gui.cma \
     $(TOP)/commons/commons_features.cma 


INCLUDEDIRS=$(TOP)/commons $(TOP)/globals \
	$(TOP)/h_program-lang \
	$(TOP)/graph_code \
	$(TOP)/h_visualization \
	$(TOP)/h_program-visual \
	$(TOP)/lang_php/parsing \
        $(TOP)/lang_php/analyze \
         $(TOP)/lang_php/analyze/foundation \
         $(TOP)/lang_php/analyze/checker \
         $(TOP)/lang_php/analyze/visual \
        $(TOP)/lang_php/matcher \
        $(TOP)/lang_ml/parsing \
         $(TOP)/lang_ml/analyze \
         $(TOP)/lang_ml/analyze/visual \
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
        $(TOP)/lang_rust/parsing \
         $(TOP)/lang_rust/analyze \
        $(TOP)/lang_opa/parsing \
         $(TOP)/lang_opa/analyze \
        $(TOP)/lang_java/parsing \
         $(TOP)/lang_java/analyze \
        $(TOP)/lang_erlang/parsing \
         $(TOP)/lang_erlang/analyze \
        $(TOP)/lang_html/parsing \
         $(TOP)/lang_html/analyze \
        $(TOP)/lang_text \


SYSINCLUDES=\
  -I $(TOP)/external/ocamlgtk/src \
  -I $(TOP)/external/ocamlcairo/src \

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma 
all.opt:: $(TARGET).cmxa
opt: all.opt


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)



EXTRA_FLAGS=-w -3
cairo_helpers.cmo: cairo_helpers.ml
	$(OCAMLC) $(EXTRA_FLAGS) -c $<

cairo_helpers.cmx: cairo_helpers.ml
	$(OCAMLOPT) $(PP) $(EXTRA_FLAGS) -c $<


clean::
	rm -f $(TARGET).top


##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################

TEXMAIN=Visual.tex.nw
TEX=Visual.tex

# set of noweb (.nw) files

# must be in the same order of the #include for syncweb multi files support
# to work
SRC_ORIG=Visual.tex.nw

# set of ml files, the views. Also used by make lpclean.
SRC_NOT_SYNCED=custom_list_generic.ml
SRC_ML= $(filter-out $(SRC_NOT_SYNCED), $(SRC))
SRC_NO_MLI=flag_visual.mli
SRC_MLI=$(filter-out $(SRC_NO_MLI), $(SRC_ML:ml=mli))

SRC_VIEWS=$(SRC_MLI) $(SRC_ML) ../main_codemap.ml

#------------------------------------------------------------------------------
sync:
	for i in $(SRC_VIEWS); do echo $$i; $(SYNCWEB) $(SRC_ORIG) $$i || exit 1; done 


pdf:
	$(NOWEB) $(TEXMAIN) > $(TEX)
	pdflatex $(TEX)
#	pdflatex $(TEX)

lpclean::
	rm -f $(TEX)

lpdistclean::
	rm -f $(SRC_VIEWS) .md5sum_* $(TEX)

clean::
	rm -f *.aux *.toc *.log *.brf *.out

