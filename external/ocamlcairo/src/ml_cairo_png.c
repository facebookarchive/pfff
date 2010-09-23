/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#if CAIRO_HAS_PNG_FUNCTIONS
static value
_ml_cairo_image_surface_create_from_png_stream (value f, cairo_bool_t unsafe)
{
  CAMLparam1(f);
  CAMLlocal1(c);
  cairo_surface_t *surf;

  c = caml_alloc_small (2, 0);
  Field (c, 0) = f;
  Field (c, 1) = Val_unit;
  surf = cairo_image_surface_create_from_png_stream (unsafe ? ml_cairo_unsafe_read_func : ml_cairo_read_func, 
						     &c);
  if (Is_exception_result (Field (c, 1)))
    caml_raise (Extract_exception (Field (c, 1)));

  CAMLreturn (Val_cairo_surface_t (surf));
}

CAMLprim value
ml_cairo_image_surface_create_from_png_stream_unsafe (value f)
{
  return _ml_cairo_image_surface_create_from_png_stream (f, 1);
}

CAMLprim value
ml_cairo_image_surface_create_from_png_stream (value f)
{
  return _ml_cairo_image_surface_create_from_png_stream (f, 0);
}


static value
_ml_cairo_surface_write_to_png_stream (value surf, value f, cairo_bool_t unsafe)
{
  CAMLparam2(surf, f);
  CAMLlocal1(c);
  cairo_status_t s;

  c = caml_alloc_small (2, 0);
  Field (c, 0) = f;
  Field (c, 1) = Val_unit;

  s = cairo_surface_write_to_png_stream (cairo_surface_t_val (surf),
					 unsafe ? ml_cairo_unsafe_write_func : ml_cairo_write_func,
					 &c);
  if (Is_exception_result (Field (c, 1)))
    caml_raise (Extract_exception (Field (c, 1)));
  cairo_treat_status (s);

  CAMLreturn (Val_unit);
}

CAMLprim value
ml_cairo_surface_write_to_png_stream_unsafe (value surf, value f)
{
  return _ml_cairo_surface_write_to_png_stream (surf, f, 1);
}

CAMLprim value
ml_cairo_surface_write_to_png_stream (value surf, value f)
{
  return _ml_cairo_surface_write_to_png_stream (surf, f, 0);
}

#else

Cairo_Unsupported(cairo_image_surface_create_from_png_stream_unsafe,	"PNG functions not supported")
Cairo_Unsupported(cairo_image_surface_create_from_png_stream,		"PNG functions not supported")
Cairo_Unsupported(cairo_surface_write_to_png_stream_unsafe,		"PNG functions not supported")
Cairo_Unsupported(cairo_surface_write_to_png_stream,			"PNG functions not supported")

#endif
