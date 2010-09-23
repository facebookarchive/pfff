/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

wMake_Val_final_pointer(cairo_surface_t, cairo_surface_destroy, 0)

cairo_content_t
cairo_content_t_val (value v)
{
  switch (Long_val (v))
    {
    case 0: return CAIRO_CONTENT_COLOR;
    case 1: return CAIRO_CONTENT_ALPHA;
    case 2: return CAIRO_CONTENT_COLOR_ALPHA;
    default: assert (0);
    }
}

value
Val_cairo_content_t (cairo_content_t c)
{
  switch (c)
    {
    case CAIRO_CONTENT_COLOR      : return Val_long(0);
    case CAIRO_CONTENT_ALPHA      : return Val_long(1);
    case CAIRO_CONTENT_COLOR_ALPHA: return Val_long(2);
    default: assert(0);
    }
}

wML_4(cairo_surface_create_similar, \
     cairo_surface_t_val, cairo_content_t_val, \
     Int_val, Int_val, Val_cairo_surface_t)

/* surface_reference */
/* surface_destroy */

CAMLprim value
ml_cairo_surface_finish (value surf)
{
  cairo_surface_finish (cairo_surface_t_val (surf));
  check_surface_status (surf);
  return Val_unit;
}

#define Val_cairo_surface_type_t(v)  Val_int(v)
wML_1 (cairo_surface_get_type, cairo_surface_t_val, Val_cairo_surface_type_t)
wML_1 (cairo_surface_get_content, cairo_surface_t_val, Val_cairo_content_t)

static void
ml_cairo_destroy_user_data (void *data)
{
  caml_remove_global_root (data);
  caml_stat_free (data);
}

void
ml_cairo_surface_set_stream_data (cairo_surface_t *surf, value *root)
{
  static const cairo_user_data_key_t ml_cairo_stream_data_key;

  cairo_status_t s;

  s = cairo_surface_set_user_data (surf, 
				   &ml_cairo_stream_data_key, root, 
				   ml_cairo_destroy_user_data);
  if (s != CAIRO_STATUS_SUCCESS) 
    {
      cairo_surface_destroy (surf);
      ml_cairo_destroy_user_data (root);
      cairo_treat_status (s);
    }
}

void
ml_cairo_surface_set_image_data (cairo_surface_t *surf, value v)
{
  static const cairo_user_data_key_t ml_cairo_image_data_key;

  cairo_status_t s;
  value *root;

  root = ml_cairo_make_root (v);

  s = cairo_surface_set_user_data (surf, 
				   &ml_cairo_image_data_key, root, 
				   ml_cairo_destroy_user_data);
  if (s != CAIRO_STATUS_SUCCESS) 
    {
      cairo_surface_destroy (surf);
      ml_cairo_destroy_user_data (root);
      cairo_treat_status (s);
    }
}

/* surface_get_user_data */

wML_2(cairo_surface_get_font_options, cairo_surface_t_val, cairo_font_options_t_val, Unit)

wML_1(cairo_surface_flush, cairo_surface_t_val, Unit)

wML_1(cairo_surface_mark_dirty, cairo_surface_t_val, Unit)

wML_5(cairo_surface_mark_dirty_rectangle, cairo_surface_t_val, Int_val, Int_val, Int_val, Int_val, Unit)

wML_3(cairo_surface_set_device_offset, cairo_surface_t_val, Double_val, Double_val, Unit)
CAMLprim value
ml_cairo_surface_get_device_offset (value s)
{
  double x, y;
  CAMLparam1(s);
  CAMLlocal1(v);
  cairo_surface_get_device_offset (cairo_surface_t_val(s), &x, &y);
  v = caml_alloc_tuple(2);
  Store_field(v, 0, caml_copy_double(x));
  Store_field(v, 1, caml_copy_double(y));
  CAMLreturn (v);
}

wML_3(cairo_surface_set_fallback_resolution, cairo_surface_t_val, Double_val, Double_val, Unit)


/* Image surface */
wML_3(cairo_image_surface_create, cairo_format_t_val, Int_val, Int_val, Val_cairo_surface_t)

/* cairo_image_surface_create_for_data -> in ml_cairo_bigarr.c */
/* cairo_image_surface_get_data  -> in ml_cairo_bigarr.c */

wML_1 (cairo_image_surface_get_format, cairo_surface_t_val, Val_cairo_format_t)
wML_1 (cairo_image_surface_get_width, cairo_surface_t_val, Val_int)
wML_1 (cairo_image_surface_get_height, cairo_surface_t_val, Val_int)
wML_1 (cairo_image_surface_get_stride, cairo_surface_t_val, Val_int)

