/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include <caml/bigarray.h>

#include "ml_cairo.h"

unsigned long bigarray_byte_size (struct caml_bigarray *);

CAMLprim value
ml_bigarray_byte_size (value b)
{
  return Val_long (bigarray_byte_size (Bigarray_val (b)));
}

CAMLprim value
ml_bigarray_kind_float (value v)
{
  struct caml_bigarray *ba = Bigarray_val (v);

  switch (ba->flags & BIGARRAY_KIND_MASK)
    {
    case BIGARRAY_FLOAT32:
    case BIGARRAY_FLOAT64:
    case BIGARRAY_COMPLEX32:
    case BIGARRAY_COMPLEX64:
      return Val_true;
    default:
      return Val_false;
    }
}

CAMLprim value
ml_cairo_image_surface_create_for_data (value img, value fmt, value w, value h, value stride)
{
  cairo_surface_t *surf;
  surf = cairo_image_surface_create_for_data (Data_bigarray_val (img),
					      cairo_format_t_val (fmt),
					      Int_val (w),
					      Int_val (h),
					      Int_val (stride));
  ml_cairo_surface_set_image_data (surf, img);

  return Val_cairo_surface_t (surf);
}

/* cairo_image_surface_get_data */
