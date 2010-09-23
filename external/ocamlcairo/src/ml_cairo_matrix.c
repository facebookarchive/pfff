/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#ifdef ARCH_ALIGN_DOUBLE
void
ml_convert_cairo_matrix_in (value v, cairo_matrix_t *mat)
{
  mat->xx = Double_field (v, 0);
  mat->yx = Double_field (v, 1);
  mat->xy = Double_field (v, 2);
  mat->yy = Double_field (v, 3);
  mat->x0 = Double_field (v, 4);
  mat->y0 = Double_field (v, 5);
}

value
ml_convert_cairo_matrix_out (const cairo_matrix_t *mat)
{
  value v;
  v = caml_alloc_small (6 * Double_wosize, Double_array_tag);
  Store_double_field (v, 0, mat->xx);
  Store_double_field (v, 1, mat->yx);
  Store_double_field (v, 2, mat->xy);
  Store_double_field (v, 3, mat->yy);
  Store_double_field (v, 4, mat->x0);
  Store_double_field (v, 5, mat->y0);
  return v;
}
#endif

/* matrix_init */
/* matrix_init_identity */
/* matrix_init_translate */
/* matrix_init_scale */
/* matrix_init_rotate */

CAMLprim value
ml_cairo_matrix_translate (value m, value x, value y)
{
#ifndef ARCH_ALIGN_DOUBLE
  CAMLparam3(m, x, y);
  value v = cairo_matrix_alloc();
  cairo_copy_matrix (v, m);
  cairo_matrix_translate (cairo_matrix_t_val (v), Double_val (x), Double_val (y));
  CAMLreturn (v);
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (m, &mat);
  cairo_matrix_translate (&mat, Double_val (x), Double_val (y));
  return ml_convert_cairo_matrix_out (&mat);
#endif
}

CAMLprim value
ml_cairo_matrix_scale (value m, value x, value y)
{
#ifndef ARCH_ALIGN_DOUBLE
  CAMLparam3(m, x, y);
  value v = cairo_matrix_alloc();
  cairo_copy_matrix (v, m);
  cairo_matrix_scale (cairo_matrix_t_val (v), Double_val (x), Double_val (y));
  CAMLreturn (v);
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (m, &mat);
  cairo_matrix_scale (&mat, Double_val (x), Double_val (y));
  return ml_convert_cairo_matrix_out (&mat);
#endif
}

CAMLprim value
ml_cairo_matrix_rotate (value m, value a)
{
#ifndef ARCH_ALIGN_DOUBLE
  CAMLparam2(m, a);
  value v = cairo_matrix_alloc();
  cairo_copy_matrix (v, m);
  cairo_matrix_rotate (cairo_matrix_t_val (v), Double_val (a));
  CAMLreturn (v);
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (m, &mat);
  cairo_matrix_rotate (&mat, Double_val (a));
  return ml_convert_cairo_matrix_out (&mat);
#endif
}

CAMLprim value
ml_cairo_matrix_invert (value m)
{
#ifndef ARCH_ALIGN_DOUBLE
  CAMLparam1(m);
  value v = cairo_matrix_alloc();
  cairo_copy_matrix (v, m);
  cairo_matrix_invert (cairo_matrix_t_val (v));
  CAMLreturn (v);
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (m, &mat);
  cairo_matrix_invert (&mat);
  return ml_convert_cairo_matrix_out (&mat);
#endif
}

CAMLprim value
ml_cairo_matrix_multiply (value a, value b)
{
#ifndef ARCH_ALIGN_DOUBLE
  CAMLparam2(a, b);
  value r = cairo_matrix_alloc();
  cairo_matrix_multiply (cairo_matrix_t_val (r),
			 cairo_matrix_t_val (a),
			 cairo_matrix_t_val (b));
  CAMLreturn (r);
#else
  cairo_matrix_t r, m_a, m_b;
  ml_convert_cairo_matrix_in (a, &m_a);
  ml_convert_cairo_matrix_in (b, &m_b);
  cairo_matrix_multiply (&r, &m_a, &m_b);
  return ml_convert_cairo_matrix_out (&r);
#endif
}

CAMLprim value
ml_cairo_matrix_transform_distance (value m, value p)
{
  double x = Double_field (p, 0);
  double y = Double_field (p, 1);
#ifndef ARCH_ALIGN_DOUBLE
  cairo_matrix_transform_distance (cairo_matrix_t_val (m), &x, &y);
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (m, &mat);
  cairo_matrix_transform_distance (&mat, &x, &y);
#endif
  return ml_cairo_point (x, y);
}

CAMLprim value
ml_cairo_matrix_transform_point (value m, value p)
{
  double x = Double_field (p, 0);
  double y = Double_field (p, 1);
#ifndef ARCH_ALIGN_DOUBLE
  cairo_matrix_transform_point (cairo_matrix_t_val (m), &x, &y);
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (m, &mat);
  cairo_matrix_transform_point (&mat, &x, &y);
#endif
  return ml_cairo_point (x, y);
}
