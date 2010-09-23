
include ../config.make

TARGETS = basket kapow
ifdef LABLGTKDIR
TARGETS += text demo spline knockout font cube
endif
ifdef LIBSVG_CAIRO_CFLAGS
TARGETS += svg2png 
endif

all : $(TARGETS)

svg2png : svg2png.ml
	$(OCAMLOPT) -o $@ -I ../src cairo.cmxa svg_cairo.cmxa $^

kapow : kapow.ml
	$(OCAMLOPT) -o $@ -I ../src cairo.cmxa $^

font : font.ml
	$(OCAMLOPT) -o $@ -I ../src cairo.cmxa $^

text : text.ml
	$(OCAMLOPT) -w s -o $@ -I ../src -I $(LABLGTKDIR) lablgtk.cmxa cairo.cmxa cairo_lablgtk.cmxa gtkInit.cmx $^

demo : demo.ml
	$(OCAMLOPT) -w s -o $@ -I ../src -I $(LABLGTKDIR) lablgtk.cmxa cairo.cmxa cairo_lablgtk.cmxa gtkInit.cmx $^

cube : cube.ml
	$(OCAMLOPT) -o $@ -I ../src -I $(LABLGTKDIR) lablgtk.cmxa cairo.cmxa cairo_lablgtk.cmxa gtkInit.cmx $^

spline : spline.ml
	$(OCAMLOPT) -w s -o $@ -I ../src -I $(LABLGTKDIR) lablgtk.cmxa cairo.cmxa cairo_lablgtk.cmxa gtkInit.cmx $^

basket : basket.ml
	$(OCAMLOPT) -o $@ -I ../src bigarray.cmxa cairo.cmxa $^

basket.b : basket.ml
	$(OCAMLC) -g -o $@ -I ../src bigarray.cma cairo.cma $^

knockout : knockout.ml
	$(OCAMLOPT) -o $@ -I ../src -I $(LABLGTKDIR) cairo.cmxa lablgtk.cmxa cairo_lablgtk.cmxa gtkInit.cmx $^

clean :
	rm -f *.cm* *.o $(TARGETS) *.ps *.ppm *.png

include ../Makefile.rules
