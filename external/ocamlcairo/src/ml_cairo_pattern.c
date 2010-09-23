/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#define W_CHECK_STATUS check_pattern_status
#define W_CONV_CAIRO   cairo_pattern_t_val

#include "ml_cairo.h"

wMake_Val_final_pointer(cairo_pattern_t, cairo_pattern_destroy, 0)

CAMLprim value
ml_cairo_pattern_create_rgb (value r, value g, value b)
{
  cairo_pattern_t *p = cairo_pattern_create_rgb (Double_val (r), Double_val (g), Double_val (b));
  cairo_treat_status (cairo_pattern_status (p));
  return Val_cairo_pattern_t (p);
}

CAMLprim value
ml_cairo_pattern_create_rgba (value r, value g, value b, value a)
{
  cairo_pattern_t *p = cairo_pattern_create_rgba (Double_val (r), Double_val (g), 
						  Double_val (b), Double_val (a));
  cairo_treat_status (cairo_pattern_status (p));
  return Val_cairo_pattern_t (p);
}

CAMLprim value
ml_cairo_pattern_create_for_surface (value surf)
{
  cairo_pattern_t *p = cairo_pattern_create_for_surface (cairo_surface_t_val (surf));
  cairo_treat_status (cairo_pattern_status (p));
  return Val_cairo_pattern_t (p);
}

CAMLprim value
ml_cairo_pattern_create_linear (value x0, value y0, value x1, value y1)
{
  cairo_pattern_t *p = cairo_pattern_create_linear (Double_val (x0), Double_val (y0), 
						    Double_val (x1), Double_val (y1));
  cairo_treat_status (cairo_pattern_status (p));
  return Val_cairo_pattern_t (p);
}

CAMLprim value
ml_cairo_pattern_create_radial (value cx0, value cy0, value r0,
				value cx1, value cy1, value r1)
{
  cairo_pattern_t *p = cairo_pattern_create_radial (Double_val (cx0), Double_val (cy0), Double_val (r0), 
						    Double_val (cx1), Double_val (cy1), Double_val (r1));
  cairo_treat_status (cairo_pattern_status (p));
  return Val_cairo_pattern_t (p);
}
wML_bc6(cairo_pattern_create_radial)

/* pattern_reference */
/* pattern_destroy */

#define Val_cairo_pattern_type_t(v) Val_int(v)
wML_1(cairo_pattern_get_type, cairo_pattern_t_val, Val_cairo_pattern_type_t)

wML_4_cairo(pattern_add_color_stop_rgb,  Double_val, Double_val, Double_val, Double_val)
wML_5_cairo(pattern_add_color_stop_rgba, Double_val, Double_val, Double_val, Double_val, Double_val)

CAMLprim value
ml_cairo_pattern_set_matrix (value p, value m)
{
#ifdef ARCH_ALIGN_DOUBLE
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (m, &mat);
  cairo_pattern_set_matrix (cairo_pattern_t_val (p), &mat);
#else
  cairo_pattern_set_matrix (cairo_pattern_t_val (p), cairo_matrix_t_val (m));
#endif
  check_pattern_status (p);
  return Val_unit;
}

CAMLprim value
ml_cairo_pattern_get_matrix (value p)
{
#ifdef ARCH_ALIGN_DOUBLE
  cairo_matrix_t mat;
  cairo_pattern_get_matrix (cairo_pattern_t_val (p), &mat);
  check_pattern_status (p);
  return ml_convert_cairo_matrix_out (&mat);
#else
  CAMLparam1(p);
  value m = caml_alloc_small (6 * Double_wosize, Double_array_tag);
  cairo_pattern_get_matrix (cairo_pattern_t_val (p), cairo_matrix_t_val (m));
  check_pattern_status (p);
  CAMLreturn (m);
#endif
}

#define cairo_extend_t_val(v) ((cairo_extend_t) Int_val(v))
#define Val_cairo_extend_t(v) Val_int(v)

wML_1_cairo(pattern_set_extend, cairo_extend_t_val)
wML_1(cairo_pattern_get_extend, cairo_pattern_t_val, Val_cairo_extend_t)

#define cairo_filter_t_val(v) ((cairo_filter_t) Int_val(v))
#define Val_cairo_filter_t(v) Val_int(v)

wML_1_cairo(pattern_set_filter, cairo_filter_t_val)
wML_1(cairo_pattern_get_filter, cairo_pattern_t_val, Val_cairo_filter_t)
