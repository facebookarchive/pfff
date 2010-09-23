/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#define CAML_NAME_SPACE

#include "ml_cairo.h"

#include <svg-cairo.h>

static value ml_svg_cairo_status (svg_cairo_status_t) Noreturn;

static value
ml_svg_cairo_status (svg_cairo_status_t s)
{
  static value *exn;
  assert (s != SVG_CAIRO_STATUS_SUCCESS);

  if (s == SVG_CAIRO_STATUS_NO_MEMORY)
    caml_raise_out_of_memory ();

  if (exn == NULL)
    {
      exn = caml_named_value ("svg_cairo_status_exn");
      if (exn == NULL)
	caml_failwith ("svg-cairo exception");
    }

  caml_raise_with_arg (*exn, Val_int (s));
}

#define check_svg_cairo_status(s)	if (s != SVG_CAIRO_STATUS_SUCCESS) ml_svg_cairo_status (s)

wMake_Val_final_pointer (svg_cairo_t, svg_cairo_destroy, 0)
#define svg_cairo_t_val(v)	wPointer_val(svg_cairo_t, v)

CAMLprim value
ml_svg_cairo_create (value unit)
{
  svg_cairo_status_t status;
  svg_cairo_t *s;
  status = svg_cairo_create (&s);
  check_svg_cairo_status (status);
  return Val_svg_cairo_t (s);
}

CAMLprim value
ml_svg_cairo_parse (value v, value f)
{
  svg_cairo_status_t status;
  status = svg_cairo_parse (svg_cairo_t_val (v), String_val (f));
  check_svg_cairo_status (status);
  return Val_unit;
}

CAMLprim value
ml_svg_cairo_parse_buffer (value v, value b)
{
  svg_cairo_status_t status;
  status = svg_cairo_parse_buffer (svg_cairo_t_val (v), 
				   String_val (b),
				   caml_string_length (b));
  check_svg_cairo_status (status);
  return Val_unit;
}

CAMLprim value
ml_svg_cairo_parse_chunk_begin (value v)
{
  svg_cairo_status_t status;
  status = svg_cairo_parse_chunk_begin (svg_cairo_t_val (v));
  check_svg_cairo_status (status);
  return Val_unit;
}

CAMLprim value
ml_svg_cairo_parse_chunk (value v, value b, value o, value l)
{
  svg_cairo_status_t status;
  if (Unsigned_int_val (o) + Unsigned_int_val (l) > caml_string_length (b))
    caml_invalid_argument ("Svg_cairo.parse_chunk: invalid substring");
  status = svg_cairo_parse_chunk (svg_cairo_t_val (v), 
				  String_val (b) + Unsigned_int_val (o),
				  Unsigned_int_val (l));
  check_svg_cairo_status (status);
  return Val_unit;
}

CAMLprim value
ml_svg_cairo_parse_chunk_end (value v)
{
  svg_cairo_status_t status;
  status = svg_cairo_parse_chunk_end (svg_cairo_t_val (v));
  check_svg_cairo_status (status);
  return Val_unit;
}

CAMLprim value
ml_svg_cairo_render (value v, value cr)
{
  svg_cairo_status_t status;
  status = svg_cairo_render (svg_cairo_t_val (v), cairo_t_val (cr));
  check_svg_cairo_status (status);
  return Val_unit;
}

CAMLprim value
ml_svg_cairo_set_viewport_dimension (value v, value w, value h)
{
  svg_cairo_status_t status;
  status = svg_cairo_set_viewport_dimension (svg_cairo_t_val (v),
					     Unsigned_int_val (w),
					     Unsigned_int_val (h));
  check_svg_cairo_status (status);
  return Val_unit;
}

CAMLprim value
ml_svg_cairo_get_size (value s)
{
  unsigned int w, h;
  value r;
  svg_cairo_get_size (svg_cairo_t_val (s), &w, &h);
  r = caml_alloc_small (2, 0);
  Field (r, 0) = Val_long (w);
  Field (r, 1) = Val_long (h);
  return r;
}
