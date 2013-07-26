# $Id: Makefile 1518 2010-06-25 09:23:44Z garrigue $
# Makefile for lablgtk.

COMPILER = $(CAMLC) $(MLFLAGS) $(MLBYTEFLAGS) $(OCAMLCFLAGS_EXTRA) -w s -c
LINKER = $(CAMLC) $(MLFLAGS) $(MLBYTEFLAGS)
COMPOPT = $(CAMLOPT) $(MLFLAGS) -w s -c
LINKOPT = $(CAMLOPT) $(MLFLAGS)
LIBRARIAN = $(CAMLMKLIB) -verbose -ocamlc "$(CAMLC)" -ocamlopt "$(CAMLOPT)"
TOPLEVEL = $(CAMLMKTOP) $(MLFLAGS)
CAMLP4 = $(CAMLP4O) pr_o.cmo

CONFIG = ../config.make

# protect against missing definitions
BINDIR = ""
LIBDIR = ""
INSTALLDIR = $(LIBDIR)/lablgtk2
DLLDIR = $(LIBDIR)/stublibs

include $(CONFIG)

TARGETS = varcc$(XE) lablgtktop$(XE) lablgtk2$(XB) gdk_pixbuf_mlsource$(XE)

all: byte
world: byte $(CAMLBEST)

MLLIBS = lablgtk.cma
CLIBS = liblablgtk2$(XA)
#MLLINK = unix.cma str.cma

# For -DG_LOG_DOMAIN=\"LablGTK\"
ifneq ($(TOOLCHAIN),msvc)
GTKCFLAGS += -imacros ml_domain.h
else
GTKCFLAGS += /FI ml_domain.h
endif

# compile using a custom version of mlvalues.h, where value is abstract
ifdef ABSVALUE
GTKCFLAGS += -Iabsvalue -DABSVALUE
endif

ifdef DEBUG
CFLAGS = -g $(GTKCFLAGS)
CUSTOM = -custom
#MLLINK += -cclib -lcamlrund
MLBYTEFLAGS = -g -dtypes
else
CFLAGS = -DG_DISABLE_ASSERT -DG_DISABLE_CAST_CHECKS $(GTKCFLAGS)
ifneq ($(TOOLCHAIN),msvc)
CFLAGS += -O
endif
endif

ifeq ($(THREADS_LIB),system)
THFLAGS = -thread
else
THFLAGS = -vmthread
CUSTOM = -custom
endif
THLINK = unix.cma threads.cma

ifdef USE_CC
CCOMPILER = $(CC) -c -I"$(LIBDIR)" $(CFLAGS)
else
CCOMPILER = $(CAMLC) -c -ccopt '$(CFLAGS)' -verbose
endif

ifdef USE_GL
ifdef LABLGLDIR
MLFLAGS += -I $(LABLGLDIR)
endif
MLLINK += lablgl.cma
MLLIBS += lablgtkgl.cma
CLIBS += liblablgtkgl2$(XA)
GLMLOBJS = glGtk.cmo
GLCOBJS = ml_gtkgl$(XO)
endif

#ifdef USE_GNOME
#MLLIBS += lablgnome.cma
#CLIBS += liblablgnome$(XA)
#GNOMEMLOBJS = gtkXmHTML.cmo gHtml.cmo
#GNOMECOBJS = ml_gtkxmhtml$(XO)
#endif

ifdef USE_GLADE
MLLIBS += lablglade.cma
CLIBS += liblablglade2$(XA)
GLADEMLOBJS = glade.cmo
GLADECOBJS = ml_glade$(XO)
TARGETS += lablgladecc$(XE)
endif

ifdef USE_GNOMECANVAS
MLLIBS += lablgnomecanvas.cma
CLIBS += liblablgnomecanvas$(XA)
GNOMECANVASMLOBJS = gnomeCanvas.cmo gnoCanvas.cmo
GNOMECANVASCOBJS = ml_gnomecanvas$(XO)
endif

ifdef USE_GNOMEUI
MLLIBS += lablgnomeui.cma
CLIBS += liblablgnomeui$(XA)
GNOMEUIMLOBJS = gnomeDruid.cmo gnoDruid.cmo
GNOMEUICOBJS = ml_gnomedruid$(XO)

ifdef USE_PANEL
MLLIBS += lablpanel.cma
CLIBS += liblablpanel$(XA)
PANELMLOBJS = panel.cmo
PANELCOBJS = ml_panel$(XO)
endif
endif


ifdef USE_RSVG
MLLIBS += lablrsvg.cma
CLIBS += liblablrsvg$(XA)
RSVGMLOBJS = rsvg.cmo
RSVGCOBJS = ml_rsvg$(XO)
$(RSVGCOBJS) : CFLAGS+=$(HAVE_SVGZ)
endif

ifdef USE_GTKSPELL
MLLIBS += lablgtkspell.cma
CLIBS += liblablgtkspell$(XA)
GTKSPELLMLOBJS = gtkSpell.cmo
GTKSPELLCOBJS = ml_gtkspell$(XO)
endif

ifdef USE_GTKSOURCEVIEW
byte:: lablgtksourceview.cma liblablgtksourceview$(XA)
opt:: lablgtksourceview.cmxa liblablgtksourceview$(XA)
ifeq ($(HAS_NATIVE_DYNLINK),yes)
opt::  lablgtksourceview.cmxs
endif

ml_gtksourceview.o: CFLAGS=$(GTKSOURCEVIEWCFLAGS)
GTKSOURCEVIEWMLOBJS = gtkSourceView.cmo gSourceView.cmo
GTKSOURCEVIEWCOBJS = ml_gtksourceview$(XO)
ml_gtksourceview.c: pango_tags.h
GTKSOURCEVIEWMLOBJSENUM = sourceViewEnums.cmo 
GTKSOURCEVIEWPROPS = gtkSourceView.props
CLIBS += liblablgtksourceview$(XA)
EXTRA_MLLIBS += lablgtksourceview.cma
EXTRA_OBJS += $(GTKSOURCEVIEWMLOBJS) $(GTKSOURCEVIEWMLOBJSENUM) 
BEFORE_DEPEND += $(GTKSOURCEVIEWMLOBJS:%.cmo=%.ml) $(GTKSOURCEVIEWPROPS:%.props=%Props.ml) $(GTKSOURCEVIEWPROPS:%.props= o%Props.ml) $(GTKSOURCEVIEWMLOBJSENUM:%.cmo=%.ml)
lablgtksourceview.cma liblablgtksourceview$(XA): \
	$(GTKSOURCEVIEWCOBJS) $(GTKSOURCEVIEWMLOBJSENUM) $(GTKSOURCEVIEWPROPS:%.props=%Props.cmo) $(GTKSOURCEVIEWPROPS:%.props=o%Props.cmo) $(GTKSOURCEVIEWMLOBJS)
	$(LIBRARIAN) -o lablgtksourceview $^ $(GTKSOURCEVIEWLIBS)
lablgtksourceview.cmxa: $(GTKSOURCEVIEWCOBJS) $(GTKSOURCEVIEWMLOBJSENUM:.cmo=.cmx) $(GTKSOURCEVIEWPROPS:%.props=%Props.cmx) $(GTKSOURCEVIEWPROPS:%.props=o%Props.cmx) $(GTKSOURCEVIEWMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablgtksourceview $^ $(GTKSOURCEVIEWLIBS)

endif
ifdef USE_GTKSOURCEVIEW2
MLLIBS += lablgtksourceview2.cma
CLIBS += liblablgtksourceview2$(XA)
GTKSOURCEVIEW2MLOBJS = gtkSourceView2.cmo gSourceView2.cmo
GTKSOURCEVIEW2COBJS = ml_gtksourceview2$(XO)
GTKSOURCEVIEW2MLOBJSENUM = sourceView2Enums.cmo 
GTKSOURCEVIEW2PROPS = gtkSourceView2.props
BEFORE_DEPEND += $(GTKSOURCEVIEW2PROPS:%.props=%Props.ml) $(GTKSOURCEVIEW2PROPS:%.props= o%Props.ml) $(GTKSOURCEVIEWMLOBJSENUM:%.cmo=%.ml)

EXTRA_OBJS += $(GTKSOURCEVIEW2MLOBJSENUM)
lablgtksourceview2.cma liblablgtksourceview2$(XA): \
	$(GTKSOURCEVIEW2COBJS) $(GTKSOURCEVIEW2MLOBJSENUM) $(GTKSOURCEVIEW2PROPS:%.props=%Props.cmo) $(GTKSOURCEVIEW2PROPS:%.props=o%Props.cmo) $(GTKSOURCEVIEW2MLOBJS)
	$(LIBRARIAN) -o lablgtksourceview2 $^ $(GTKSOURCEVIEW2LIBS)
lablgtksourceview2.cmxa: $(GTKSOURCEVIEW2COBJS) $(GTKSOURCEVIEW2MLOBJSENUM:.cmo=.cmx) $(GTKSOURCEVIEW2PROPS:%.props=%Props.cmx) $(GTKSOURCEVIEW2PROPS:%.props=o%Props.cmx) $(GTKSOURCEVIEW2MLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablgtksourceview2 $^ $(GTKSOURCEVIEW2LIBS)
lablgtksourceview2.cmxs: DYNLINKLIBS=$(GTKSOURCEVIEW2_LIBS)
endif

ifdef USE_GTKQUARTZ
CFLAGS += -DHAS_GTKQUARTZ
endif
# Rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .cmxa .c $(XO) .d$(XO) .var .h .opt .def .ml4
.c$(XO):
	$(CCOMPILER) $<
.cpp$(XO):
	$(CCOMPILER) $<
.c.d$(XO):
	$(CAMLC) -c -ccopt '-MT -DCAML_DLL -Fo$@ $(CFLAGS)' $<
.ml.cmo:
	$(COMPILER) $<
.mli.cmi:
	$(COMPILER) $<
.ml.cmx:
	$(COMPOPT) $<
.ml4.cmo:
	$(CAMLC) -c -pp "$(CAMLP4O) -impl" -impl $<
.cmxa.cmxs:
	$(CAMLOPT) -verbose -o $@ -shared -linkall -I . \
        -ccopt '$(filter -L%, $(DYNLINKLIBS))' $<

#.ml4.ml:
#	$(CAMLP4) -impl $< -o $@

%_tags.h %_tags.c %Enums.ml: %_tags.var varcc$(XE)
	./varcc $<

%Props.ml o%Props.ml: %.props propcc$(XE)
	./propcc $<

# Files
GTKOBJS =  ml_gtk$(XO) ml_pango$(XO) ml_gtkaction$(XO) \
	ml_gtkbin$(XO) ml_gtkbroken$(XO) ml_gtkbutton$(XO) \
	ml_gtkassistant$(XO) \
	ml_gtkedit$(XO) ml_gtkfile$(XO) ml_gtklist$(XO) \
	ml_gtkmenu$(XO) ml_gtkmisc$(XO) \
	ml_gtkpack$(XO) ml_gtkrange$(XO) ml_gtkstock$(XO) \
	ml_gtktext$(XO) ml_gtktree$(XO)
COBJS = ml_gdkpixbuf$(XO) ml_gdk$(XO) ml_glib$(XO) ml_gobject$(XO) \
	ml_gpointer$(XO) ml_gvaluecaml$(XO) wrappers$(XO) $(GTKOBJS)
GTKPROPS += gtkBase.props gtkBin.props gtkButton.props gtkEdit.props \
	gtkList.props gtkMenu.props gtkMisc.props gtkPack.props \
	gtkRange.props gtkText.props gtkTree.props gtkFile.props \
	gtkAction.props gtkBroken.props gtkAssistant.props
MLOBJS1 =  gaux.cmo gpointer.cmo gutf8.cmo glib.cmo gobject.cmo
ENUMOBJS += gdkEnums.cmo pangoEnums.cmo gtkEnums.cmo
MLOBJS2 = pango.cmo gdk.cmo gdkEvent.cmo gdkKeysyms.cmo gdkPixbuf.cmo \
	gtk.cmo gtkSignal.cmo gtkStock.cmo gtkObject.cmo
MLOBJS3 = gtkData.cmo gtkBase.cmo gtkPack.cmo gtkButton.cmo \
	gtkAssistant.cmo \
	gtkMenu.cmo gtkMisc.cmo gtkWindow.cmo gtkList.cmo \
	gtkBin.cmo gtkEdit.cmo gtkRange.cmo gtkText.cmo gtkTree.cmo \
	gtkFile.cmo gtkMain.cmo gtkBroken.cmo \
	gPango.cmo gDraw.cmo gObj.cmo gData.cmo gMain.cmo
MLOBJS4 = gContainer.cmo gPack.cmo gButton.cmo gText.cmo \
	gMenu.cmo gMisc.cmo gTree.cmo gList.cmo gFile.cmo gWindow.cmo \
	gAssistant.cmo \
	gBin.cmo gEdit.cmo gRange.cmo gAction.cmo gBroken.cmo \
	gUtil.cmo gToolbox.cmo
MLOBJS =  $(MLOBJS1) $(ENUMOBJS) $(MLOBJS2) $(GTKPROPS:.props=Props.cmo) \
	$(MLOBJS3) $(GTKPROPS:%.props=o%Props.cmo) $(MLOBJS4)
MLXOBJS = $(MLOBJS1) $(ENUMOBJS) $(MLOBJS2) $(MLOBJS3) $(MLOBJS4)
THOBJS = gtkThread.cmo
INITOBJS = gtkInit.cmo
THINITOBJS = gtkThInit.cmo
EXTRAOBJS = $(INITOBJS) $(THOBJS) $(THINITOBJS)
ALLOBJS = $(MLXOBJS) $(GLMLOBJS) $(GNOMEMLOBJS) $(GLADEMLOBJS) \
	$(RSVGMLOBJS) $(GNOMECANVASMLOBJS) $(GNOMEUIMLOBJS) $(PANELMLOBJS) \
	$(GTKSPELLMLOBJS) $(GTKSOURCEVIEW2MLOBJS) \
	$(INITOBJS)
ALLTHOBJS = $(THOBJS) $(THINITOBJS)
PROPOBJS = $(GTKPROPS:%.props=%Props.cmo) $(GTKPROPS:%.props=o%Props.cmo)
BEFORE_DEPEND += gtkSignal.ml

# Targets
byte:: $(TARGETS) $(INITOBJS)
opt::  $(TARGETS) lablgtkopt

depend:
	$(MAKE) real-depend USE_GL=1 USE_GLADE=1 USE_RSVG=1 \
	  USE_GNOMECANVAS=1 USE_GNOMEUI=1 USE_PANEL=1 USE_GTKSPELL=1 \
	  USE_GTKSOURCEVIEW=1 USE_GTKSOURCEVIEW2=1
real-depend: $(ENUMOBJS:.cmo=.ml) $(PROPOBJS:.cmo=.ml) $(BEFORE_DEPEND)
	ocamldep $(ALLOBJS:.cmo=.ml) $(PROPOBJS:.cmo=.ml) \
	  $(ALLTHOBJS:.cmo=.ml) $(BEFORE_DEPEND) *.mli > .depend

ifeq ($(THREADS_LIB),no)
lablgtktop$(XE): $(MLLIBS)
	$(TOPLEVEL) $(CUSTOM) -o $@ $(MLLINK) -I . $(MLLIBS)
else
lablgtktop$(XE): $(MLLIBS) $(ALLTHOBJS)
	$(TOPLEVEL) $(THFLAGS) $(CUSTOM) -o $@ $(THLINK) $(MLLINK) \
	   -I . $(MLLIBS) $(THOBJS)
endif

lablgtk2: Makefile $(CONFIG) lablgtk2.in
	sed -e "s|@INSTALLDIR@|$(INSTALLDIR)|g" \
	    -e "s|@LABLGLDIR@|$(LABLGLDIR)|g" \
	    -e "s|@LIBDIR@|$(LIBDIR)|g" \
	    -e "s|@MLLIBS@|$(MLLINK) $(MLLIBS)|g" \
	    -e "s|@INITOBJS@|$(INITOBJS)|g" \
	    -e "s|@THOBJS@|$(THOBJS)|g" \
	    -e "s|@THINITOBJS@|$(THINITOBJS)|g" \
	    -e "s|@USEDLL@|$(HAS_DLL_SUPPORT)|g" \
	    -e "s|@THREADS_LIB@|$(THREADS_LIB)|g" \
	    < lablgtk2.in > $@
	chmod 755 $@

lablgtk2.bat: lablgtk2.bat.in
	cp lablgtk2.bat.in lablgtk2.bat

build.ml: Makefile $(CONFIG) build.ml.in
	sed -e "s|@LABLGTK_MLS@|$(MLOBJS:.cmo=)|" \
	    -e "s|@EXTRA_MLS@|$(EXTRAOBJS:.cmo=)|" \
	    -e "s|@PROP_MLS@|$(PROPOBJS:.cmo=)|" \
	    -e "s|@GTKLIBS@|$(GTKLIBS)|" \
	    -e "s|@GLADE_MLS@|$(GLADEMLOBJS:.cmo=)|" \
	    -e "s|@GLADE_LIBS@|$(GLADELIBS)|" \
	    -e "s|@RSVG_MLS@|$(RSVGMLOBJS:.cmo=)|" \
	    -e "s|@RSVG_LIBS@|$(RSVGLIBS)|" \
	    -e "s|@CANVAS_MLS@|$(GNOMECANVASMLOBJS:.cmo=)|" \
	    -e "s|@CANVAS_LIBS@|$(GNOMECANVASLIBS)|" \
	    -e "s|@TOOLCHAIN@|$(TOOLCHAIN)|" \
	    < build.ml.in > $@

gdk_pixbuf_mlsource$(XE) : lablgtk.cma gdk_pixbuf_mlsource.ml
	$(CAMLC) -o $@ -I . $^

lablgtkopt:: $(MLLIBS:.cma=.cmxa) $(INITOBJS:.cmo=.cmx)
ifeq ($(THREADS_LIB),system)
lablgtkopt:: $(THOBJS:.cmo=.cmx)
endif

ifeq ($(HAS_NATIVE_DYNLINK),yes)
lablgtkopt::  $(MLLIBS:.cma=.cmxs)
endif

LABLGLADECCOBJS = gutf8.cmo xml2_lexer.cmo lablgladecc.cmo

lablgladecc$(XE): $(LABLGLADECCOBJS)
	$(LINKER) -o $@ $(LABLGLADECCOBJS)

testcc$(XE): lablgladecc$(XE) lablgtktop
	./lablgladecc -test > testcc.ml
	./lablgtktop testcc.ml
	rm -f testcc.ml

PREOBJS= $(MLOBJS) $(EXTRAOBJS) $(GLADEMLOBJS) $(RSVGMLOBJS) \
	$(GNOMECANVASMLOBJS)

preinstall: build.ml
	mkdir -p "$(INSTALLDIR)" "$(BINDIR)" "$(DLLDIR)"
	cp $(PREOBJS:.cmo=.ml) "$(INSTALLDIR)"
	for f in $(PREOBJS:.cmo=.mli); do \
	  if test -f $$f; then cp $$f "$(INSTALLDIR)"; fi; done
	cp build.ml "$(INSTALLDIR)"
	cp lablgtk2$(XB) gdk_pixbuf_mlsource$(XE) "$(BINDIR)"
	cp lablgladecc$(XE) "$(BINDIR)/lablgladecc2$(XE)"
	cp $(CLIBS) "$(INSTALLDIR)"
	cp $(CLIBS:lib%$(XA)=dll%$(XS)) "$(DLLDIR)"

install:
	mkdir -p "$(INSTALLDIR)" "$(BINDIR)" "$(DLLDIR)"
	cp $(ALLOBJS:.cmo=.cmi) $(EXTRA_OBJS:.cmo=.cmi) "$(INSTALLDIR)"
	cp -p *.mli "$(INSTALLDIR)"
	cp -p $(ALLOBJS:.cmo=.ml) $(ALLTHOBJS:.cmo=.ml) $(EXTRA_OBJS:.cmo=.ml) "$(INSTALLDIR)"
	cp $(EXTRA_MLLIBS) $(MLLIBS) $(THOBJS) $(INITOBJS) $(THINITOBJS) "$(INSTALLDIR)"
	cp $(CLIBS) "$(INSTALLDIR)"
	cd "$(INSTALLDIR)" && $(RANLIB) $(CLIBS)
	cp varcc$(XE) propcc$(XE) "$(INSTALLDIR)"
	if test $(THREADS_LIB) != no; \
	   then cp  $(ALLTHOBJS:.cmo=.cmi) "$(INSTALLDIR)"; \
	fi
	if test $(THREADS_LIB) = vm || test $(HAS_DLL_SUPPORT) != yes; \
	   then cp lablgtktop$(XE) "$(INSTALLDIR)"; \
	fi
	cp -p *.h "$(INSTALLDIR)"
	@if test -f lablgtk.cmxa; then $(MAKE) installopt; fi
	@if test -f dlllablgtk2$(XS); then $(MAKE) installdll; fi
	cp lablgtk2$(XB) "$(BINDIR)"
	if test -f lablgladecc$(XE); then \
	   cp lablgladecc$(XE) "$(BINDIR)/lablgladecc2$(XE)"; fi
	cp gdk_pixbuf_mlsource$(XE) "$(BINDIR)"

installdll:
	cp $(CLIBS:lib%$(XA)=dll%$(XS)) "$(DLLDIR)" || \
	   echo "Couldn't install dlls in default location: $(DLLDIR)"
	if test -f dlllablgtk2$(XA); then \
	  cp $(CLIBS:lib%$(XA)=dll%$(XA)) "$(INSTALLDIR)"; fi

installopt:
	cp $(MLLIBS:.cma=.cmxa) $(MLLIBS:.cma=$(XA)) $(EXTRA_MLLIBS:.cma=.cmxa) $(EXTRA_MLLIBS:.cma=$(XA)) "$(INSTALLDIR)"
ifeq ($(HAS_NATIVE_DYNLINK),yes)
	cp $(MLLIBS:.cma=.cmxs) $(EXTRA_MLLIBS:.cma=.cmxs) "$(INSTALLDIR)"
endif
	cd "$(INSTALLDIR)" && $(RANLIB) $(MLLIBS:.cma=$(XA)) $(EXTRA_MLLIBS:.cma=$(XA))
	cp $(ALLOBJS:.cmo=.cmx) $(EXTRA_OBJS:.cmo=.cmx) "$(INSTALLDIR)"
	cp $(INITOBJS:.cmo=$(XO)) "$(INSTALLDIR)"
	if test -f gtkThread.cmx; then \
	   cp $(THOBJS:.cmo=.cmx) $(THOBJS:.cmo=$(XO)) "$(INSTALLDIR)"; fi

lablgtk.cma liblablgtk2$(XA): $(COBJS) $(MLOBJS)
	$(LIBRARIAN) -o lablgtk -oc lablgtk2 $^ $(GTKLIBS)
lablgtk.cmxa: $(COBJS) $(MLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablgtk -oc lablgtk2 $^ $(GTKLIBS)
lablgtk.cmxs: DYNLINKLIBS=$(GTK_LIBS)

lablgtkgl.cma liblablgtkgl2$(XA): $(GLCOBJS) $(GLMLOBJS)
	$(LIBRARIAN) -o lablgtkgl -oc lablgtkgl2 $^ $(GTKGLLIBS)
lablgtkgl.cmxa: $(GLCOBJS) $(GLMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablgtkgl -oc lablgtkgl2 $^ $(GTKGLLIBS)
lablgtkgl.cmxs: DYNLINKLIBS=$(GTKGL_LIBS)

lablglade.cma liblablglade2$(XA): $(GLADECOBJS) $(GLADEMLOBJS)
	$(LIBRARIAN) -o lablglade -oc lablglade2 $^ $(GLADELIBS)
lablglade.cmxa: $(GLADECOBJS) $(GLADEMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablglade -oc lablglade2 $^ $(GLADELIBS)
lablglade.cmxs: DYNLINKLIBS=$(GLADE_LIBS)

lablrsvg.cma liblablrsvg$(XA): $(RSVGCOBJS) $(RSVGMLOBJS)
	$(LIBRARIAN) -o lablrsvg $^ $(RSVGLIBS)
lablrsvg.cmxa: $(RSVGCOBJS) $(RSVGMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablrsvg $^ $(RSVGLIBS)
lablrsvg.cmxs: DYNLINKLIBS=$(RSVG_LIBS)

lablgnomecanvas.cma liblablgnomecanvas$(XA): \
		$(GNOMECANVASCOBJS) $(GNOMECANVASMLOBJS)
	$(LIBRARIAN) -o lablgnomecanvas $^ $(GNOMECANVASLIBS)
lablgnomecanvas.cmxa: $(GNOMECANVASCOBJS) $(GNOMECANVASMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablgnomecanvas $^ $(GNOMECANVASLIBS)
# gnoCanvas.cmo: COMPILER+= -i
lablgnomecanvas.cmxs: DYNLINKLIBS=$(GNOMECANVAS_LIBS)

lablgnomeui.cma liblablgnomeui$(XA): $(GNOMEUICOBJS) $(GNOMEUIMLOBJS)
	$(LIBRARIAN) -o lablgnomeui $^ $(GNOMEUILIBS)
lablgnomeui.cmxa: $(GNOMEUICOBJS) $(GNOMEUIMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablgnomeui $^ $(GNOMEUILIBS)
# gnoDruid.cmo: COMPILER+= -i
lablgnomeui.cmxs: DYNLINKLIBS=$(GNOMEUI_LIBS)

lablpanel.cma liblablpanel$(XA): $(PANELCOBJS) $(PANELMLOBJS)
	$(LIBRARIAN) -o lablpanel $^ $(PANELLIBS)
lablpanel.cmxa: $(PANELCOBJS) $(PANELMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablpanel $^ $(PANELLIBS)
lablpanel.cmxs: DYNLINKLIBS=$(PANEL_LIBS)

lablgtkspell.cma liblablgtkspell$(XA): $(GTKSPELLCOBJS) $(GTKSPELLMLOBJS)
	$(LIBRARIAN) -o lablgtkspell $^ $(GTKSPELLLIBS)
lablgtkspell.cmxa: $(GTKSPELLCOBJS) $(GTKSPELLMLOBJS:.cmo=.cmx)
	$(LIBRARIAN) -o lablgtkspell $^ $(GTKSPELLLIBS)
lablgtkspell.cmxs: DYNLINKLIBS=$(GTKSPELL_LIBS)

gtkSignal.ml: gtkSignal.ml4
	$(CAMLP4O) pa_macro.cmo $(HAS_PRINTEXC_BACKTRACE) -impl $< -o $@

gtkThread.cmo: gtkThread.ml
	$(COMPILER) $(THFLAGS) gtkThread.ml

gtkThread.cmi: gtkThread.mli
	$(COMPILER) $(THFLAGS) gtkThread.mli

gtkThread.cmx: gtkThread.ml
	$(COMPOPT) $(THFLAGS) gtkThread.ml

xml2_lexer.ml: xml2_lexer.mll
	$(CAMLLEX) xml2_lexer.mll
xml2_lexer.cmo xml2_lexer.cmx : xml2_lexer.cmi

varcc$(XE): varcc.cmo
	$(LINKER) -o $@ $<
	rm -f *_tags.h *_tags.c

propcc$(XE): propcc.cmo
	$(LINKER) -o $@ $<

check_externals$(XE): check_externals.cmo
	$(LINKER) -o $@ $<

clean:
	rm -f *.cm* *.o *.a *.so *.exe *.obj *.lib *.dll *_tags.[ch] *.gch \
	  $(TARGETS) varcc.ml gtkSignal.ml xml2_lexer.ml \
	  propcc$(XE) check_externals$(XE) check_externals.ml propcc.ml \
	  *Enums.ml *Props.ml
	rm -f \#*\# *~


DOCFILES = gPango.ml gDraw.mli gObj.mli gMain.mli gData.mli \
	gContainer.mli gBin.mli gPack.mli gButton.mli gText.mli \
        gMenu.mli gMisc.mli gTree.mli gFile.mli gList.mli gWindow.mli \
        gEdit.mli gRange.mli gAction.mli gUtil.mli gToolbox.mli gBroken.mli \
	gaux.ml gpointer.mli glib.mli gobject.mli \
        gdkEnums.ml pangoEnums.ml gtkEnums.ml \
        gdk.mli gdkEvent.ml gdkPixbuf.mli \
        gtk.ml pango.ml gtkSignal.mli gtkObject.ml \
        gtkStock.ml gtkData.ml gtkBase.ml gtkPack.ml gtkButton.ml \
        gtkMenu.ml gtkMisc.ml gtkWindow.ml gtkList.ml \
        gtkEdit.ml gtkRange.ml gtkText.ml gtkTree.ml \
        gtkFile.ml gtkMain.ml \
        $(GTKPROPS:%.props=%Props.ml)
ifdef USE_GLADE
DOCFILES += glade.mli xml2_lexer.mli
endif
ifdef USE_GL
DOCFILES += glGtk.mli
endif
ifdef USE_RSVG
DOCFILES += rsvg.mli
endif
ifdef USE_GNOMECANVAS
DOCFILES += gnomeCanvas.ml gnoCanvas.mli
endif
ifdef USE_GNOMEUI
DOCFILES += gnomeDruid.ml gnoDruid.mli
endif
ifdef USE_PANEL
DOCFILES += panel.mli
endif
ifdef USE_GTKSPELL
DOCFILES += gtkSpell.mli
endif
ifdef USE_GTKSOURCEVIEW
DOCFILES += gSourceView.mli
endif
ifdef USE_GTKSOURCEVIEW2
DOCFILES += gSourceView2.mli
endif

OCAMLDOC = ocamldoc
lablgtk$(XO)doc : $(DOCFILES) $(ALLOBJS)
	$(OCAMLDOC) -dump $@ -v $(if $(LABLGLDIR),-I $(LABLGLDIR)) $(DOCFILES)
gtkdoc.cmo : gtkdoc.ml
	$(CAMLC) -I +ocamldoc -pp "$(CAMLP4O) pa_macro.cmo $(ODOC_DEF)" -c $<
DOCDIR=../doc
DOC_URI=file:///usr/share/gtk-doc/html
DOC_BASE_URI=$(if $(DOC_URI),-base-uri $(DOC_URI))
doc : lablgtk$(XO)doc gtkdoc.cmo
	mkdir -p $(DOCDIR)/html
	$(OCAMLDOC) -g ./gtkdoc.cmo $(DOC_BASE_URI) -v -t LablGTK -d $(DOCDIR)/html -load $<
doc_texi : lablgtk$(XO)doc
	$(OCAMLDOC) -texi -v -t LablGTK -o $(DOCDIR)/lablgtk2.texi -load $<

$(GTKOBJS): pango_tags.h gtk_tags.h ml_gtk.h gdk_tags.h ml_gdk.h \
	    ml_gobject.h ml_glib.h wrappers.h
$(GNOMEUICOBJS) : gnomeui_tags.h
$(PANELCOBJS) : panel_tags.h
$(GTKSOURCEVIEWCOBJS):  ml_glib.h ml_gdk.h ml_gtk.h ml_gobject.h ml_gdkpixbuf.h ml_pango.h \
			gtk_tags.h gdk_tags.h ml_gtktext.h sourceView_tags.h wrappers.h

$(GTKSOURCEVIEW2COBJS):  ml_glib.h ml_gdk.h ml_gtk.h ml_gobject.h ml_gdkpixbuf.h ml_pango.h \
			gtk_tags.h gdk_tags.h ml_gtktext.h sourceView2_tags.h wrappers.h

ml_glib$(XO): glib_tags.h ml_glib.h wrappers.h
ml_gobject$(XO): gobject_tags.h ml_gvaluecaml.h wrappers.h
ml_gvaluecaml$(XO): ml_gobject.h wrappers.h
ml_gdk$(XO): gdk_tags.h ml_gdk.h ml_gpointer.h \
	pango_tags.h ml_pango.h wrappers.h
ml_gtk$(XO) ml_gtktext$(XO): ml_pango.h
ml_gtkgl$(XO): gtkgl_tags.h ml_gtk.h ml_gdk.h wrappers.h
ml_gtkxmhtml$(XO): gtkxmhtml_tags.h ml_gtk.h ml_gdk.h wrappers.h
ml_gpointer$(XO): ml_gpointer.h
ml_gdkpixbuf$(XO): ml_gdkpixbuf.h gdkpixbuf_tags.h ml_gdk.h gdk_tags.h \
	wrappers.h ml_gpointer.h
ml_pango$(XO): pango_tags.h ml_pango.h
ml_rsvg$(XO): ml_gobject.h ml_gdkpixbuf.h wrappers.h
ml_gnomecanvas$(XO): ml_gtk.h ml_gobject.h wrappers.h
ml_gnomedruid$(XO): ml_gtk.h ml_gobject.h wrappers.h
ml_panel$(XO): ml_gtk.h ml_gobject.h wrappers.h

varcc.cmo: varcc.ml4

-include .depend
