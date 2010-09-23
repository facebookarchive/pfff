AC_INIT(CAIRO_OCAML, 1.2.0)
AC_CONFIG_SRCDIR(src/cairo.ml)
AC_CONFIG_AUX_DIR(support)

# Check for OCaml programs 
AC_PROG_OCAML()

# Check for cairo
PKG_CHECK_MODULES(CAIRO, cairo >= 1.2 freetype2)

# Optional GTK support (for the X11 backend)
AC_ARG_WITH(gtk, 
	    AS_HELP_STRING([--with-gtk],[Cairo/GTK+ integration via LablGTK]),
	    use_gtk=$withval, use_gtk=yes)
AC_ARG_VAR(LABLGTKDIR,[Location of the LablGTK library])

if test $use_gtk = yes ; then
  # Check for LablGTK
  AC_CHECK_OCAML_MODULE(lablgtk, LABLGTKDIR, Gobject, +lablgtk2 +lablgtk)

  if test "$LABLGTKDIR" ; then
    # Check for gdk-pixbuf
    PKG_CHECK_MODULES(GDK, cairo gdk-2.0 >= 2.8 gdk-pixbuf-2.0, :, use_gtk=no)
  else
    use_gtk=no
  fi
fi

# Optional libsvg-cairo support
PKG_CHECK_MODULES(LIBSVG_CAIRO, libsvg-cairo, use_libsvg_cairo=yes, use_libsvg_cairo=no)

echo
echo "  GTK+         support: $use_gtk"
echo "  libsvg-cairo support: $use_libsvg_cairo"
echo

AC_OUTPUT(config.make)
