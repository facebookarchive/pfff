
VERSION = 1.2.0

OCAMLC     = ocamlc.opt
OCAMLOPT   = ocamlopt.opt
OCAMLMKLIB = ocamlmklib
OCAMLLIB   = /home/pad/packages/MacOS/stow/godi-3.11/lib/ocaml/std-lib
OCAMLDOC   = ocamldoc
OCAMLDEP   = ocamldep

INSTALLDIR = $(OCAMLLIB)/cairo

LABLGTKDIR = +lablgtk2
C_LABLGTKDIR = $(subst +,$(OCAMLLIB)/,$(LABLGTKDIR))

# stop ocamlmklib moaning
FILT = -Wl,--export-dynamic

CAIRO_CFLAGS = -I/opt/local/include/cairo -I/opt/local/include/pixman-1 -I/opt/local/include/freetype2 -I/opt/local/include -I/opt/local/include/libpng12  
CAIRO_LIBS   = $(filter-out $(FILT),-L/opt/local/lib -lcairo -lpixman-1 -lfontconfig -lexpat -liconv -lpng12 -lXrender -lX11 -lXau -lXdmcp -lfreetype -lz  )

GDK_CFLAGS = -D_REENTRANT -I/opt/local/include/cairo -I/opt/local/include/pixman-1 -I/opt/local/include/freetype2 -I/opt/local/include -I/opt/local/include/libpng12 -I/opt/local/include/gtk-2.0 -I/opt/local/lib/gtk-2.0/include -I/opt/local/include/pango-1.0 -I/opt/local/include/gio-unix-2.0/ -I/opt/local/include/glib-2.0 -I/opt/local/lib/glib-2.0/include  
GDK_LIBS   = $(filter-out $(FILT),-L/opt/local/lib -lgdk-x11-2.0 -lpangocairo-1.0 -lXinerama -lXi -lXrandr -lXcursor -lXcomposite -lXdamage -lpangoft2-1.0 -lpango-1.0 -lgio-2.0 -lXext -lXfixes -lcairo -lpixman-1 -lfontconfig -lexpat -lfreetype -lpng12 -lz -lXrender -lX11 -lXau -lXdmcp -lgdk_pixbuf-2.0 -lm -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lglib-2.0 -lintl -liconv  )

LIBSVG_CAIRO_CFLAGS = 
LIBSVG_CAIRO_LIBS   = 

cobjs     = $(patsubst %.c, %.o, $(filter %.c,$(1)))
mlintfs   = $(patsubst %.mli, %.cmi, $(filter %.mli,$(1)))
mlobjs    = $(patsubst %.ml, %.cmo, $(filter %.ml,$(1)))
mloptobjs = $(patsubst %.ml, %.cmx, $(filter %.ml,$(1)))
