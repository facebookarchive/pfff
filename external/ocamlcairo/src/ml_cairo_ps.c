/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#if CAIRO_HAS_PS_SURFACE
# include <cairo-ps.h>

static value
_ml_cairo_ps_surface_create_for_stream (value f, value w, value h, cairo_bool_t unsafe)
{
  CAMLparam3(f, w, h);
  value *c;
  cairo_surface_t *surf;

  c = ml_cairo_make_closure (f);
  surf = cairo_ps_surface_create_for_stream (unsafe ? ml_cairo_unsafe_write_func : ml_cairo_write_func,
					     c, Double_val (w), Double_val (h));

  ml_cairo_surface_set_stream_data (surf, c);

  CAMLreturn (Val_cairo_surface_t (surf));
}

CAMLprim value
ml_cairo_ps_surface_create_for_stream_unsafe (value f, value w, value h)
{
  return _ml_cairo_ps_surface_create_for_stream (f, w, h, 1);
}

CAMLprim value
ml_cairo_ps_surface_create_for_stream (value f, value w, value h)
{
  return _ml_cairo_ps_surface_create_for_stream (f, w, h, 0);
}

wML_3(cairo_ps_surface_set_size, cairo_surface_t_val, Double_val, Double_val, Unit)

wML_2(cairo_ps_surface_dsc_comment, cairo_surface_t_val, String_val, Unit)
wML_1(cairo_ps_surface_dsc_begin_setup, cairo_surface_t_val, Unit)
wML_1(cairo_ps_surface_dsc_begin_page_setup, cairo_surface_t_val, Unit)

#else

Cairo_Unsupported(cairo_ps_surface_create_for_stream_unsafe,	"PS backend not supported");
Cairo_Unsupported(cairo_ps_surface_create_for_stream,		"PS backend not supported");
Cairo_Unsupported(cairo_ps_surface_set_size,			"PS backend not supported");
Cairo_Unsupported(cairo_ps_surface_dsc_comment,			"PS backend not supported");
Cairo_Unsupported(cairo_ps_surface_dsc_begin_setup,		"PS backend not supported");
Cairo_Unsupported(cairo_ps_surface_dsc_begin_page_setup,	"PS backend not supported");
#endif
