/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

wML_0(cairo_version_string, caml_copy_string)
wML_0(cairo_version, Val_int)

CAMLprim value
ml_CAIRO_VERSION_STRING (value unit)
{
  return caml_copy_string (CAIRO_VERSION_STRING);
}

CAMLprim value
ml_CAIRO_VERSION (value unit)
{
  return Val_int (CAIRO_VERSION);
}

CAMLprim value
ml_CAIRO_VERSION_ENCODE (value maj, value min, value mic)
{
  return Val_int (CAIRO_VERSION_ENCODE (Int_val(maj), Int_val(min), Int_val(mic)));
}

wMake_Val_final_pointer(cairo_t, cairo_destroy, 0)

CAMLprim value
ml_cairo_create (value surf)
{
  cairo_t *p = cairo_create (cairo_surface_t_val (surf));
  cairo_treat_status (cairo_status (p));
  return Val_cairo_t (p);
}

/* cairo_reference */
/* cairo_destroy   */

wML_0_cairo(save)

wML_0_cairo(restore)

wML_0_cairo(push_group)
wML_1_cairo(push_group_with_content, cairo_content_t_val)
CAMLprim value
ml_cairo_pop_group (value cr)
{
  cairo_pattern_t *p = cairo_pop_group (cairo_t_val (cr));
  check_cairo_status (cr);
  return Val_cairo_pattern_t (p);
}
wML_0_cairo(pop_group_to_source)


#define cairo_operator_t_val(v) ((cairo_operator_t) Int_val(v))
#define Val_cairo_operator_t(v) Val_int(v)

wML_1_cairo(set_operator, cairo_operator_t_val)

wML_1_cairo(set_source, cairo_pattern_t_val)

wML_3_cairo(set_source_rgb, Double_val, Double_val, Double_val)

wML_4_cairo(set_source_rgba, Double_val, Double_val, Double_val, Double_val)

wML_3_cairo(set_source_surface, cairo_surface_t_val, Double_val, Double_val)

wML_1_cairo(set_tolerance, Double_val)

wML_1_cairo(set_antialias, cairo_antialias_t_val)

#define cairo_fill_rule_t_val(v) ((cairo_fill_rule_t) Int_val(v))
#define Val_cairo_fill_rule_t(v) Val_int(v)

wML_1_cairo(set_fill_rule, cairo_fill_rule_t_val)

wML_1_cairo(set_line_width, Double_val)

#define cairo_line_cap_t_val(v) ((cairo_line_cap_t) Int_val(v))
#define Val_cairo_line_cap_t(v) Val_int(v)

wML_1_cairo(set_line_cap, cairo_line_cap_t_val)

#define cairo_line_join_t_val(v) ((cairo_line_join_t) Int_val(v))
#define Val_cairo_line_join_t(v) Val_int(v)

wML_1_cairo(set_line_join, cairo_line_join_t_val)

CAMLprim value
ml_cairo_set_dash (value cr, value d, value off)
{
#ifndef ARCH_ALIGN_DOUBLE
  cairo_set_dash (cairo_t_val (cr), Double_array_val (d),
		  Double_array_length (d), Double_val (off));
#else
  int i, ndash = Double_array_length (d);
  double *dashes = caml_stat_alloc (ndash * sizeof (double));
  for (i = 0; i < ndash; i++)
    dashes[i] = Double_field (d, i);
  cairo_set_dash (cairo_t_val (cr), dashes, ndash, Double_val (off));
  caml_stat_free (dashes);
#endif
  check_cairo_status (cr);
  return Val_unit;
}

wML_1_cairo(set_miter_limit, Double_val)

wML_2_cairo(translate, Double_val, Double_val)

wML_2_cairo(scale, Double_val, Double_val)

wML_1_cairo(rotate, Double_val)

CAMLprim value
ml_cairo_transform (value v_cr, value v_matrix)
{
#ifndef ARCH_ALIGN_DOUBLE
  cairo_transform (cairo_t_val (v_cr), cairo_matrix_t_val (v_matrix));
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (v_matrix, &mat);
  cairo_transform (cairo_t_val (v_cr), &mat);
#endif
  check_cairo_status (v_cr);
  return Val_unit;
}

CAMLprim value
ml_cairo_set_matrix (value v_cr, value v_matrix)
{
#ifndef ARCH_ALIGN_DOUBLE
  cairo_set_matrix (cairo_t_val (v_cr), cairo_matrix_t_val (v_matrix));
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (v_matrix, &mat);
  cairo_set_matrix (cairo_t_val (v_cr), &mat);
#endif
  check_cairo_status (v_cr);
  return Val_unit;
}

wML_0_cairo (identity_matrix)

value
ml_cairo_point (double x, double y)
{
  value p;
  p = caml_alloc_small (2 * Double_wosize, Double_array_tag);
  Store_double_field (p, 0, x);
  Store_double_field (p, 1, y);
  return p;
}

CAMLprim value
ml_cairo_user_to_device (value cr, value p)
{
  double x, y;
  x = Double_field (p, 0);
  y = Double_field (p, 1);
  cairo_user_to_device (cairo_t_val (cr), &x, &y);
  check_cairo_status (cr);
  return ml_cairo_point (x, y);
}

CAMLprim value
ml_cairo_user_to_device_distance (value cr, value p)
{
  double x, y;
  x = Double_field (p, 0);
  y = Double_field (p, 1);
  cairo_user_to_device_distance (cairo_t_val (cr), &x, &y);
  check_cairo_status (cr);
  return ml_cairo_point (x, y);
}

CAMLprim value
ml_cairo_device_to_user (value cr, value p)
{
  double x, y;
  x = Double_field (p, 0);
  y = Double_field (p, 1);
  cairo_device_to_user (cairo_t_val (cr), &x, &y);
  check_cairo_status (cr);
  return ml_cairo_point (x, y);
}

CAMLprim value
ml_cairo_device_to_user_distance (value cr, value p)
{
  double x, y;
  x = Double_field (p, 0);
  y = Double_field (p, 1);
  cairo_device_to_user_distance (cairo_t_val (cr), &x, &y);
  check_cairo_status (cr);
  return ml_cairo_point (x, y);
}

wML_0_cairo(new_path)

wML_2_cairo(move_to, Double_val, Double_val)

wML_0_cairo(new_sub_path)

wML_2_cairo(line_to, Double_val, Double_val)

wML_6_cairo(curve_to, Double_val, Double_val, Double_val, Double_val, Double_val, Double_val)

wML_5_cairo(arc, Double_val, Double_val, Double_val, Double_val, Double_val)

wML_5_cairo(arc_negative, Double_val, Double_val, Double_val, Double_val, Double_val)

wML_2_cairo(rel_move_to, Double_val, Double_val)

wML_2_cairo(rel_line_to, Double_val, Double_val)

wML_6_cairo(rel_curve_to, Double_val, Double_val, Double_val, Double_val, Double_val, Double_val)

wML_4_cairo(rectangle, Double_val, Double_val, Double_val, Double_val)

wML_0_cairo(close_path)

wML_0_cairo(paint)

wML_1_cairo(paint_with_alpha, Double_val)

wML_1_cairo(mask, cairo_pattern_t_val)

wML_3_cairo(mask_surface, cairo_surface_t_val, Double_val, Double_val)

wML_0_cairo(stroke)

wML_0_cairo(stroke_preserve)

wML_0_cairo(fill)

wML_0_cairo(fill_preserve)

wML_0_cairo(copy_page)

wML_0_cairo(show_page)

CAMLprim value
ml_cairo_in_stroke (value v_cr, value p)
{
  cairo_bool_t c_ret;
  c_ret =
    cairo_in_stroke (cairo_t_val (v_cr), Double_field (p, 0), Double_field (p, 1));
  check_cairo_status (v_cr);
  return Val_bool (c_ret);
}

CAMLprim value
ml_cairo_in_fill (value v_cr, value p)
{
  cairo_bool_t c_ret;
  c_ret =
    cairo_in_fill (cairo_t_val (v_cr), Double_field (p, 0), Double_field (p, 1));
  check_cairo_status (v_cr);
  return Val_bool (c_ret);
}

CAMLprim value
ml_cairo_stroke_extents (value v_cr)
{
  double x1, y1, x2, y2;
  cairo_stroke_extents (cairo_t_val (v_cr), &x1, &y1, &x2, &y2);
  check_cairo_status (v_cr);
  {
    CAMLparam0 ();
    CAMLlocal1 (t);
    t = caml_alloc_tuple (4);
    Store_field (t, 0, caml_copy_double (x1));
    Store_field (t, 1, caml_copy_double (y1));
    Store_field (t, 2, caml_copy_double (x2));
    Store_field (t, 3, caml_copy_double (y2));
    CAMLreturn (t);
  }
}

CAMLprim value
ml_cairo_fill_extents (value v_cr)
{
  double x1, y1, x2, y2;
  cairo_fill_extents (cairo_t_val (v_cr), &x1, &y1, &x2, &y2);
  check_cairo_status (v_cr);
  {
    CAMLparam0 ();
    CAMLlocal1 (t);
    t = caml_alloc_tuple (4);
    Store_field (t, 0, caml_copy_double (x1));
    Store_field (t, 1, caml_copy_double (y1));
    Store_field (t, 2, caml_copy_double (x2));
    Store_field (t, 3, caml_copy_double (y2));
    CAMLreturn (t);
  }
}

wML_0_cairo(reset_clip)

wML_0_cairo(clip)

wML_0_cairo(clip_preserve)


#define cairo_font_weight_t_val(v) ((cairo_font_weight_t) Int_val(v))
#define Val_cairo_font_weight_t(v) Val_int(v)

#define cairo_font_slant_t_val(v) ((cairo_font_slant_t) Int_val(v))
#define Val_cairo_font_slant_t(v) Val_int(v)

wML_3_cairo(select_font_face, String_val, cairo_font_slant_t_val, cairo_font_weight_t_val)

wML_1_cairo(set_font_size, Double_val)

CAMLprim value
ml_cairo_set_font_matrix (value v_cr, value v_matrix)
{
#ifndef ARCH_ALIGN_DOUBLE
  cairo_set_font_matrix (cairo_t_val (v_cr), cairo_matrix_t_val (v_matrix));
#else
  cairo_matrix_t mat;
  ml_convert_cairo_matrix_in (v_matrix, &mat);
  cairo_set_font_matrix (cairo_t_val (v_cr), &mat);
#endif
  check_cairo_status (v_cr);
  return Val_unit;
}

CAMLprim value
ml_cairo_get_font_matrix (value v_cr)
{
#ifndef ARCH_ALIGN_DOUBLE
  CAMLparam1(v_cr);
  value v = cairo_matrix_alloc();
  cairo_get_font_matrix (cairo_t_val (v_cr), cairo_matrix_t_val (v));
  CAMLreturn(v);
#else
  cairo_matrix_t mat;
  cairo_get_font_matrix (cairo_t_val (v_cr), &mat);
  check_cairo_status (v_cr);
  return ml_convert_cairo_matrix_out (&mat);
#endif
}

wML_1_cairo (set_font_options, cairo_font_options_t_val)
wML_1_cairo (get_font_options, cairo_font_options_t_val)

wML_1_cairo (set_scaled_font, cairo_scaled_font_t_val)

wML_1_cairo(show_text, String_val)

cairo_glyph_t *
ml_convert_cairo_glypth_in (value v, int *num_glyphs)
{
  size_t i, n = Wosize_val (v);
  cairo_glyph_t *g = caml_stat_alloc (n * sizeof (cairo_glyph_t));
  for (i = 0; i < n; i++)
    {
      value vg = Field (v, i);
      g[i].index = Unsigned_long_val (Field (vg, 0));
      g[i].x = Double_val (Field (vg, 1));
      g[i].y = Double_val (Field (vg, 2));
    }
  *num_glyphs = n;
  return g;
}

CAMLprim value
ml_cairo_show_glyphs (value v_cr, value v_glyphs)
{
  int num_glyphs;
  cairo_glyph_t *c_glyphs;
  c_glyphs = ml_convert_cairo_glypth_in (v_glyphs, &num_glyphs);
  cairo_show_glyphs (cairo_t_val (v_cr), c_glyphs, num_glyphs);
  caml_stat_free (c_glyphs);
  check_cairo_status (v_cr);
  return Val_unit;
}

wML_1 (cairo_get_font_face, cairo_t_val, Val_cairo_font_face_ref)

value
Val_cairo_font_extents (cairo_font_extents_t * s)
{
  value v = caml_alloc_small (5 * Double_wosize, Double_array_tag);
  Store_double_field (v, 0, s->ascent);
  Store_double_field (v, 1, s->descent);
  Store_double_field (v, 2, s->height);
  Store_double_field (v, 3, s->max_x_advance);
  Store_double_field (v, 4, s->max_y_advance);
  return v;
}

CAMLprim value
ml_cairo_font_extents (value cr)
{
  cairo_font_extents_t e;
  cairo_font_extents (cairo_t_val (cr), &e);
  check_cairo_status (cr);
  return Val_cairo_font_extents (&e);
}

wML_1_cairo (set_font_face, cairo_font_face_t_val)

value
Val_cairo_text_extents (cairo_text_extents_t * s)
{
  value v = caml_alloc_small (6 * Double_wosize, Double_array_tag);
  Store_double_field (v, 0, s->x_bearing);
  Store_double_field (v, 1, s->y_bearing);
  Store_double_field (v, 2, s->width);
  Store_double_field (v, 3, s->height);
  Store_double_field (v, 4, s->x_advance);
  Store_double_field (v, 5, s->y_advance);
  return v;
}

CAMLprim value
ml_cairo_text_extents (value v_cr, value v_utf8)
{
  cairo_text_extents_t c_extents;
  cairo_text_extents (cairo_t_val (v_cr), String_val (v_utf8), &c_extents);
  check_cairo_status (v_cr);
  return Val_cairo_text_extents (&c_extents);
}

CAMLprim value
ml_cairo_glyph_extents (value v_cr, value v_glyphs)
{
  int num_glyphs;
  cairo_glyph_t *c_glyphs;
  cairo_text_extents_t c_extents;
  c_glyphs = ml_convert_cairo_glypth_in (v_glyphs, &num_glyphs);
  cairo_glyph_extents (cairo_t_val (v_cr), c_glyphs, num_glyphs, &c_extents);
  caml_stat_free (c_glyphs);
  check_cairo_status (v_cr);
  return Val_cairo_text_extents (&c_extents);
}

wML_1_cairo(text_path, String_val)

CAMLprim value
ml_cairo_glyph_path (value v_cr, value v_glyphs)
{
  int num_glyphs;
  cairo_glyph_t *c_glyphs;
  c_glyphs = ml_convert_cairo_glypth_in (v_glyphs, &num_glyphs);
  cairo_glyph_path (cairo_t_val (v_cr), c_glyphs, num_glyphs);
  caml_stat_free (c_glyphs);
  check_cairo_status (v_cr);
  return Val_unit;
}


#define cairo_get(cname, conv) wML_1(cairo_get_##cname, cairo_t_val, conv)

cairo_get(operator, Val_cairo_operator_t)

cairo_get(source, Val_cairo_pattern_ref)

cairo_get(tolerance, caml_copy_double)

cairo_get(antialias, Val_cairo_antialias_t)

CAMLprim value
ml_cairo_get_current_point (value cr)
{
  double x, y;
  cairo_get_current_point (cairo_t_val (cr), &x, &y);
  return ml_cairo_point (x, y);
}

cairo_get(fill_rule, Val_cairo_fill_rule_t)

cairo_get(line_width, caml_copy_double)

cairo_get(line_cap, Val_cairo_line_cap_t)

cairo_get(line_join, Val_cairo_line_join_t)

cairo_get(miter_limit, caml_copy_double)

CAMLprim value
ml_cairo_get_matrix (value v_cr)
{
#ifndef ARCH_ALIGN_DOUBLE
  CAMLparam1(v_cr);
  value v = cairo_matrix_alloc();
  cairo_get_matrix (cairo_t_val (v_cr), cairo_matrix_t_val (v));
  CAMLreturn(v);
#else
  cairo_matrix_t mat;
  cairo_get_matrix (cairo_t_val (v_cr), &mat);
  return ml_convert_cairo_matrix_out (&mat);
#endif
}

cairo_get(target, Val_cairo_surface_ref)

cairo_get(group_target, Val_cairo_surface_ref)

/* ml_cairo_path */
/* ml_cairo_status */


value *
ml_cairo_make_root (value v)
{
  value *root = caml_stat_alloc (sizeof (value *));
  *root = v;
  caml_register_global_root (root);
  return root;
}

value *
ml_cairo_make_closure (value f)
{
  CAMLparam1(f);
  value c, *r;
  c = caml_alloc_small (2, 0);
  Field (c, 0) = f;
  Field (c, 1) = Val_unit;
  r = ml_cairo_make_root (c);
#ifdef CAMLreturnT
  CAMLreturnT (value*, r);
#else
  CAMLreturn (r);
#endif
}

cairo_status_t
ml_cairo_write_func (void *closure, const unsigned char *data, unsigned int length)
{
  value s, res, *c = closure;
  s = caml_alloc_string (length);
  memcpy (String_val (s), data, length);
  res = caml_callback_exn (Field (*c, 0), s);
  if (Is_exception_result (res))
    {
      Store_field (*c, 1, res);
      return CAIRO_STATUS_WRITE_ERROR;
    }
  return CAIRO_STATUS_SUCCESS;
}

cairo_status_t
ml_cairo_read_func (void *closure, unsigned char *data, unsigned int length)
{
  value s, res, *c = closure;
  s = caml_alloc_string (length);
  res = caml_callback_exn (Field (*c, 0), s);
  if (Is_exception_result (res))
    {
      Store_field (*c, 1, res);
      return CAIRO_STATUS_READ_ERROR;
    }
  memcpy (data, String_val (s), length);
  return CAIRO_STATUS_SUCCESS;
}

cairo_status_t
ml_cairo_unsafe_write_func (void *closure, const unsigned char *data, unsigned int length)
{
  value res, *c = closure;
  res = caml_callback2_exn (Field (*c, 0), Val_bp (data), Val_int (length));
  if (Is_exception_result (res))
    {
      Store_field (*c, 1, res);
      return CAIRO_STATUS_WRITE_ERROR;
    }
  return CAIRO_STATUS_SUCCESS;
}

cairo_status_t
ml_cairo_unsafe_read_func (void *closure, unsigned char *data, unsigned int length)
{
  value res, *c = closure;
  res = caml_callback2_exn (Field (*c, 0), Val_bp (data), Val_int (length));
  if (Is_exception_result (res))
    {
      Store_field (*c, 1, res);
      return CAIRO_STATUS_READ_ERROR;
    }
  return CAIRO_STATUS_SUCCESS;
}
