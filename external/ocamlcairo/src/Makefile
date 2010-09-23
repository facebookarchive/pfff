
include ../config.make

CPPFLAGS = -g

TARGETS = cairo
ifdef LABLGTKDIR
TARGETS += lablgtk
endif
ifdef LIBSVG_CAIRO_CFLAGS
TARGETS += svgcairo
endif
all : $(TARGETS) $(if $(OCAMLOPT),opt)
opt : $(addsuffix .opt,$(TARGETS))

cairo     : cairo.cma  libmlcairo.a
cairo.opt : cairo.cmxa dllmlcairo.so
lablgtk     : cairo_lablgtk.cma  libmlcairo_lablgtk.a
lablgtk.opt : cairo_lablgtk.cmxa dllmlcairo_lablgtk.so
gtkcairo     : gtkcairo.cma  libmlgtkcairo.a
gtkcairo.opt : gtkcairo.cmxa dllmlgtkcairo.so
svgcairo     : svg_cairo.cma libmlsvgcairo.a
svgcairo.opt : svg_cairo.cmxa dllmlsvgcairo.so

cairo_SRC = cairo.mli cairo.ml \
            cairo_bigarray.mli cairo_bigarray.ml \
	    cairo_png.mli cairo_png.ml \
	    cairo_pdf.mli cairo_pdf.ml \
	    cairo_ps.mli cairo_ps.ml \
            cairo_svg.mli cairo_svg.ml \
            cairo_ft.mli cairo_ft.ml \
            ml_cairo_wrappers.c \
            ml_cairo.c ml_cairo_status.c ml_cairo_bigarr.c ml_cairo_path.c \
	    ml_cairo_surface.c ml_cairo_pattern.c ml_cairo_matrix.c \
	    ml_cairo_font.c ml_cairo_ft.c \
	    ml_cairo_png.c ml_cairo_pdf.c ml_cairo_ps.c ml_cairo_svg.c

cairo.cma : $(call mlobjs,$(cairo_SRC))
	$(OCAMLMKLIB) -o cairo -oc mlcairo $^ $(CAIRO_LIBS)
cairo.cmxa : $(call mloptobjs,$(cairo_SRC))
	$(OCAMLMKLIB) -o cairo -oc mlcairo $^ $(CAIRO_LIBS)
libmlcairo.a dllmlcairo.so : $(call cobjs,$(cairo_SRC))
	$(OCAMLMKLIB) -o cairo -oc mlcairo $^ $(CAIRO_LIBS)

lablgtk_SRC = cairo_lablgtk.mli cairo_lablgtk.ml ml_cairo_lablgtk.c

cairo_lablgtk.cma : $(call mlobjs,$(lablgtk_SRC))
	$(OCAMLMKLIB) -o cairo_lablgtk -oc mlcairo_lablgtk $^
cairo_lablgtk.cmxa : $(call mloptobjs,$(lablgtk_SRC))
	$(OCAMLMKLIB) -o cairo_lablgtk -oc mlcairo_lablgtk $^
libmlcairo_lablgtk.a dllmlcairo_lablgtk.so : $(call cobjs,$(lablgtk_SRC))
	$(OCAMLMKLIB) -o cairo_lablgtk -oc mlcairo_lablgtk $^

svgcairo_SRC = svg_cairo.mli svg_cairo.ml ml_svg_cairo.c

svg_cairo.cma : $(call mlobjs,$(svgcairo_SRC))
	$(OCAMLMKLIB) -o svg_cairo -oc mlsvgcairo $^ $(LIBSVG_CAIRO_LIBS)
svg_cairo.cmxa : $(call mloptobjs,$(svgcairo_SRC))
	$(OCAMLMKLIB) -o svg_cairo -oc mlsvgcairo $^ $(LIBSVG_CAIRO_LIBS)
libmlsvgcairo.a dllmlsvgcairo.so : $(call cobjs,$(svgcairo_SRC))
	$(OCAMLMKLIB) -o svg_cairo -oc mlsvgcairo $^ $(LIBSVG_CAIRO_LIBS)

$(call cobjs,$(cairo_SRC))    : CPPFLAGS+=$(CAIRO_CFLAGS)
$(call cobjs,$(lablgtk_SRC))  : CPPFLAGS+=$(GDK_CFLAGS) -I$(C_LABLGTKDIR)
$(call cobjs,$(svgcairo_SRC))  : CPPFLAGS+=$(LIBSVG_CAIRO_CFLAGS)
$(call mlobjs,$(lablgtk_SRC))  : INCFLAGS=-I $(LABLGTKDIR)
$(call mlintfs,$(lablgtk_SRC))  : INCFLAGS=-I $(LABLGTKDIR)
$(call mloptobjs,$(lablgtk_SRC))  : INCFLAGS=-I $(LABLGTKDIR)

install: all
	mkdir -p $(DESTDIR)$(INSTALLDIR) $(DESTDIR)$(OCAMLLIB)/stublibs
	install -m 644 *.mli *.cmi *.cma lib*.a $(DESTDIR)$(INSTALLDIR)
ifdef OCAMLOPT
	install -m644 *.cmxa *.a *.cmx $(DESTDIR)$(INSTALLDIR)
endif
	install -m 755 dll*.so $(DESTDIR)$(INSTALLDIR)
	if test -w $(DESTDIR)$(OCAMLLIB)/stublibs ; then \
          for lib in dll*.so ; do \
            ln -s $(INSTALLDIR)/$$lib $(DESTDIR)$(OCAMLLIB)/stublibs ; done ; fi

DOCFILES = cairo.mli cairo_bigarray.mli cairo_png.mli cairo_pdf.mli cairo_ps.mli cairo_ft.mli
ifdef LABLGTKDIR
DOCFILES += cairo_lablgtk.mli
endif
ifdef LIBSVG_CAIRO_CFLAGS
DOCFILES += svg_cairo.mli
endif
doc: $(DOCFILES:%.mli=%.cmi)
	mkdir -p ../doc/html
	ocamldoc -v -html -d ../doc/html -t Cairo-ocaml $(if $(LABLGTKDIR),-I $(LABLGTKDIR)) $(DOCFILES) 

clean :
	rm -f *.cm* *.o *.a *.so .depend

.depend : $(wildcard *.ml *.mli)
	@echo "making deps"
	@$(OCAMLDEP) $^ > $@

-include .depend
-include .depend_c

depend : .depend $(wildcard *.h *.c)
	gcc -MM -isystem $(OCAMLLIB) -isystem $(C_LABLGTKDIR) $(patsubst -I%,-isystem %,$(GDK_CFLAGS)) $(filter %.c,$^) > .depend_c

.PHONY : cairo lablgtk gtkcairo doc

include ../Makefile.rules
