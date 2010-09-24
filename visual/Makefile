TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=cairo_helpers.ml \
    flag_visual.ml \
    style2.ml \
    parsing2.ml \
    editor_connection.ml \
    model2.ml \
    custom_list_generic.ml \
    completion2.ml \
    draw2.ml \
    view2.ml

#experimental:  future.ml \    ancient2.ml \      completion.ml \

SYSLIBS=str.cma unix.cma dbm.cma threads.cma  lablgtk.cma

LIBS=$(TOP)/commons/commons.cma $(TOP)/commons/commons_gui.cma \
     $(TOP)/commons/commons_features.cma \
     $(TOP)/globals/globals.cma \

INCLUDEDIRS=$(TOP)/commons $(TOP)/globals \
	$(TOP)/h_program-lang \
	$(TOP)/h_visualization \
	$(TOP)/h_program-visual \
	$(TOP)/lang_php/parsing \
        $(TOP)/lang_php/analyze \
         $(TOP)/lang_php/analyze/basic $(TOP)/lang_php/analyze/foundation \
         $(TOP)/lang_php/analyze/finder \
         $(TOP)/lang_php/analyze/checker \
         $(TOP)/lang_php/analyze/database \
        $(TOP)/lang_php/matcher \
        $(TOP)/lang_ml/parsing \
         $(TOP)/lang_ml/analyze \
        $(TOP)/lang_cpp/parsing \
         $(TOP)/lang_cpp/analyze \
        $(TOP)/lang_js/parsing \
         $(TOP)/lang_js/analyze \


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


clean::
	rm -f $(TARGET).top


##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################

# must be in the same order of the #include for syncweb multi files support
# to work

SRCNW=Visual.tex.nw
TEXMAIN=Visual.tex.nw
TEX=Visual.tex

# user oriented, public API in some sense + ml that share code with mli
LPSRC1=
LPSRCSHARED=
LPSRC2=
LPSRC=$(LPSRC1) $(LPSRCSHARED) $(LPSRC2)

#------------------------------------------------------------------------------
sync:
	for i in $(LPSRC); do $(SYNCWEB) $(SRCNW) $$i || exit 1; done 

pdf:
	$(NOWEB) $(TEXMAIN) > $(TEX)
	pdflatex $(TEX)
#	pdflatex $(TEX)

lpclean::
	rm -f $(TEX)

lpclean2::
	rm -f $(TEX) $(LPSRC) $(LPSRCOTHER) .md5sum_*

clean::
	rm -f *.aux *.toc *.log
