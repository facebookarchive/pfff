/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#if CAIRO_HAS_SVG_SURFACE
# include <cairo-svg.h>

static value
_ml_cairo_svg_surface_create_for_stream (value f, value w, value h, cairo_bool_t unsafe)
{
  CAMLparam3(f, w, h);
  value *c;
  cairo_surface_t *surf;

  c = ml_cairo_make_closure (f);
  surf = cairo_svg_surface_create_for_stream (unsafe ? ml_cairo_unsafe_write_func : ml_cairo_write_func,
					      c, Double_val (w), Double_val (h));

  ml_cairo_surface_set_stream_data (surf, c);

  CAMLreturn (Val_cairo_surface_t (surf));
}

CAMLprim value
ml_cairo_svg_surface_create_for_stream_unsafe (value f, value w, value h)
{
  return _ml_cairo_svg_surface_create_for_stream (f, w, h, 1);
}

CAMLprim value
ml_cairo_svg_surface_create_for_stream (value f, value w, value h)
{
  return _ml_cairo_svg_surface_create_for_stream (f, w, h, 0);
}

#define cairo_svg_version_t_val(v) ((cairo_svg_version_t) Int_val(v))
#define Val_cairo_svg_version_t(v) Val_int(v)

wML_2(cairo_svg_surface_restrict_to_version, cairo_surface_t_val, cairo_svg_version_t_val, Unit)
/* cairo_svg_get_versions */
wML_1(cairo_svg_version_to_string, cairo_svg_version_t_val, caml_copy_string)

#else

Cairo_Unsupported(cairo_svg_surface_create_for_stream_unsafe,	"SVG backend not supported");
Cairo_Unsupported(cairo_svg_surface_create_for_stream, 		"SVG backend not supported");
Cairo_Unsupported(cairo_svg_surface_restrict_to_version,	"SVG backend not supported");
Cairo_Unsupported(cairo_svg_version_to_string,			"SVG backend not supported");

#endif
