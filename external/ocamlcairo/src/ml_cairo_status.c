/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

wML_1 (cairo_status, cairo_t_val, Val_int)
wML_1 (cairo_surface_status, cairo_surface_t_val, Val_int)
wML_1 (cairo_pattern_status, cairo_pattern_t_val, Val_int)
wML_1 (cairo_font_face_status, cairo_font_face_t_val, Val_int)
wML_1 (cairo_scaled_font_status, cairo_scaled_font_t_val, Val_int)
wML_1 (cairo_font_options_status, cairo_font_options_t_val, Val_int)

wML_1 (cairo_status_to_string, Int_val, caml_copy_string)

void
ml_cairo_treat_status (cairo_status_t status)
{
  static value *cairo_exn;

  assert (status != CAIRO_STATUS_SUCCESS);

  if (status == CAIRO_STATUS_NO_MEMORY)
    caml_raise_out_of_memory ();

  if (cairo_exn == NULL)
    {
      cairo_exn = caml_named_value ("cairo_status_exn");
      if (cairo_exn == NULL)
	caml_failwith ("cairo exception");
    }
  caml_raise_with_arg (*cairo_exn, Val_int (status));
}
