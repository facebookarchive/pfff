/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#if CAIRO_HAS_PDF_SURFACE
# include <cairo-pdf.h>

static value
_ml_cairo_pdf_surface_create_for_stream (value f, value w, value h, cairo_bool_t unsafe)
{
  CAMLparam3(f, w, h);
  value *c;
  cairo_surface_t *surf;

  c = ml_cairo_make_closure (f);
  surf = cairo_pdf_surface_create_for_stream (unsafe ? ml_cairo_unsafe_write_func : ml_cairo_write_func,
					      c, Double_val (w), Double_val (h));

  ml_cairo_surface_set_stream_data (surf, c);

  CAMLreturn (Val_cairo_surface_t (surf));
}

CAMLprim value
ml_cairo_pdf_surface_create_for_stream_unsafe (value f, value w, value h)
{
  return _ml_cairo_pdf_surface_create_for_stream (f, w, h, 1);
}

CAMLprim value
ml_cairo_pdf_surface_create_for_stream (value f, value w, value h)
{
  return _ml_cairo_pdf_surface_create_for_stream (f, w, h, 0);
}

wML_3(cairo_pdf_surface_set_size, cairo_surface_t_val, Double_val, Double_val, Unit)

#else

Cairo_Unsupported(cairo_pdf_surface_create_for_stream_unsafe,	"PDF backend not supported");
Cairo_Unsupported(cairo_pdf_surface_create_for_stream, 		"PDF backend not supported");
Cairo_Unsupported(cairo_pdf_surface_set_size,			"PDF backend not supported");

#endif
