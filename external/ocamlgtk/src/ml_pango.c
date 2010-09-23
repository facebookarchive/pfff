/**************************************************************************/
/*                Lablgtk                                                 */
/*                                                                        */
/*    This program is free software; you can redistribute it              */
/*    and/or modify it under the terms of the GNU Library General         */
/*    Public License as published by the Free Software Foundation         */
/*    version 2, with the exception described in file COPYING which       */
/*    comes with the library.                                             */
/*                                                                        */
/*    This program is distributed in the hope that it will be useful,     */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*    GNU Library General Public License for more details.                */
/*                                                                        */
/*    You should have received a copy of the GNU Library General          */
/*    Public License along with this program; if not, write to the        */
/*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         */
/*    Boston, MA 02111-1307  USA                                          */
/*                                                                        */
/*                                                                        */
/**************************************************************************/

/* $Id: ml_pango.c 1501 2010-04-11 21:07:18Z oandrieu $ */

#include <stdio.h>
#include <pango/pango.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/custom.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_pango.h"
#include "pango_tags.h"
#include "gtk_tags.h"

#ifndef HASGTK22
#define PANGO_WRAP_WORD_CHAR -1
#endif
#include "pango_tags.c"


CAMLprim value ml_pango_init(value unit)
{
  /* Since these are declared const, must force gcc to call them! */
  GType t =
    pango_font_description_get_type();
  return Val_GType(t);
}

/* PangoFontDescription */

Make_Val_final_pointer_ext (PangoFontDescription, _new, Ignore,
                            pango_font_description_free, 20)

ML_1(pango_font_description_from_string, String_val,
     Val_PangoFontDescription_new)
ML_1(pango_font_description_copy, PangoFontDescription_val,
     Val_PangoFontDescription_new)
ML_1(pango_font_description_to_string, PangoFontDescription_val,
     copy_string_g_free)
ML_2(pango_font_description_set_family, PangoFontDescription_val,
     String_val, Unit)
ML_1(pango_font_description_get_family, PangoFontDescription_val,
     Val_string)
ML_2(pango_font_description_set_style, PangoFontDescription_val,
     Pango_style_val, Unit)
ML_1(pango_font_description_get_style, PangoFontDescription_val,
     Val_pango_style)
ML_2(pango_font_description_set_variant, PangoFontDescription_val,
     Pango_variant_val, Unit)
ML_1(pango_font_description_get_variant, PangoFontDescription_val,
     Val_pango_variant)
ML_2(pango_font_description_set_weight, PangoFontDescription_val,
     Int_val, Unit)
ML_1(pango_font_description_get_weight, PangoFontDescription_val,
     Val_int)
ML_2(pango_font_description_set_stretch, PangoFontDescription_val,
     Pango_stretch_val, Unit)
ML_1(pango_font_description_get_stretch, PangoFontDescription_val,
     Val_pango_stretch)
ML_2(pango_font_description_set_size, PangoFontDescription_val,
     Int_val, Unit)
ML_1(pango_font_description_get_size, PangoFontDescription_val,
     Val_int)


/* PangoFontMetrics */

Make_Val_final_pointer (PangoFontMetrics, pango_font_metrics_ref,
                        pango_font_metrics_unref, 20)
Make_Val_final_pointer_ext (PangoFontMetrics, _new, Ignore,
                            pango_font_metrics_unref, 20)

ML_1 (pango_font_metrics_get_ascent, PangoFontMetrics_val, Val_int)
ML_1 (pango_font_metrics_get_descent, PangoFontMetrics_val, Val_int)
ML_1 (pango_font_metrics_get_approximate_char_width,
      PangoFontMetrics_val, Val_int)
ML_1 (pango_font_metrics_get_approximate_digit_width,
      PangoFontMetrics_val, Val_int)

/* PangoFont */

#define Val_PangoFont_new(val) Val_GObject_new(G_OBJECT(val))
ML_2 (pango_font_get_metrics, PangoFont_val, PangoLanguage_val,
      Val_PangoFontMetrics_new)

/* PangoFontMap */

#define Val_PangoFontMap_new(val) Val_GObject_new(G_OBJECT(val))
ML_3 (pango_font_map_load_font, PangoFontMap_val, PangoContext_val,
      PangoFontDescription_val,
      Val_PangoFont_new)

/* Enums */

CAMLprim value ml_PANGO_SCALE ()
{
  return(Val_int(PANGO_SCALE));
}

/* This one uses the generated MLTAG but not the conversion functions because
   we have defined float values */
CAMLprim value ml_Pango_scale_val (value val) 
{
  double r;
  if (Is_block(val)) return Field(val,1); /* `CUSTOM */
  switch((long)val)
    {
    case (long)MLTAG_XX_SMALL: r = PANGO_SCALE_XX_SMALL ;break;
    case (long)MLTAG_X_SMALL:	 r = PANGO_SCALE_X_SMALL ;break;
    case (long)MLTAG_SMALL:	 r = PANGO_SCALE_SMALL ;break;
    case (long)MLTAG_MEDIUM:	 r = PANGO_SCALE_MEDIUM ;break;
    case (long)MLTAG_LARGE:	 r = PANGO_SCALE_LARGE ;break;
    case (long)MLTAG_X_LARGE:	 r = PANGO_SCALE_X_LARGE ;break;
    case (long)MLTAG_XX_LARGE: r = PANGO_SCALE_XX_LARGE ;break;
    default: printf("Bug in ml_PangoScale_val. Please report");
      r=1;
      break;
    }
  return copy_double(r);
}

/* PangoLanguage */

ML_1 (pango_language_from_string, String_val, Val_PangoLanguage)
ML_1 (pango_language_to_string, PangoLanguage_val, Val_optstring)
ML_2 (pango_language_matches, PangoLanguage_val, String_val, Val_bool)

/* PangoContext */

ML_1 (pango_context_get_font_description, PangoContext_val,
      Val_PangoFontDescription)
ML_2 (pango_context_set_font_description, PangoContext_val,
      PangoFontDescription_val, Unit)
ML_1 (pango_context_get_language, PangoContext_val, Val_PangoLanguage)
ML_2 (pango_context_set_language, PangoContext_val, PangoLanguage_val, Unit)
ML_2 (pango_context_load_font, PangoContext_val, PangoFontDescription_val,
      Val_PangoFont_new)
ML_3 (pango_context_load_fontset, PangoContext_val, PangoFontDescription_val,
      PangoLanguage_val, Val_PangoFont_new)
ML_3 (pango_context_get_metrics, PangoContext_val, PangoFontDescription_val,
      Option_val(arg3,PangoLanguage_val,NULL) Ignore, Val_PangoFontMetrics_new)

/* PangoLayout */

#define Val_PangoLayout_new(val) Val_GObject_new(G_OBJECT(val))
ML_1 (pango_layout_new, PangoContext_val, Val_PangoLayout_new)
ML_1 (pango_layout_copy, PangoLayout_val, Val_PangoLayout_new)
ML_1 (pango_layout_get_context, PangoLayout_val, Val_PangoContext)
ML_2 (pango_layout_set_text, PangoLayout_val, SizedString_val, Unit)
ML_1 (pango_layout_get_text, PangoLayout_val, Val_string)
ML_2 (pango_layout_set_markup, PangoLayout_val, SizedString_val, Unit)
ML_4 (pango_layout_set_markup_with_accel, PangoLayout_val, SizedString_val,
      Int_val, NULL Ignore, Unit)
ML_2 (pango_layout_set_font_description, PangoLayout_val,
      PangoFontDescription_val, Unit)
ML_2 (pango_layout_set_width, PangoLayout_val, Int_val, Unit)
ML_1 (pango_layout_get_width, PangoLayout_val, Val_int)
ML_2 (pango_layout_set_wrap, PangoLayout_val, Pango_wrap_mode_val, Unit)
ML_1 (pango_layout_get_wrap, PangoLayout_val, Val_pango_wrap_mode)
ML_2 (pango_layout_set_indent, PangoLayout_val, Int_val, Unit)
ML_1 (pango_layout_get_indent, PangoLayout_val, Val_int)
ML_2 (pango_layout_set_spacing, PangoLayout_val, Int_val, Unit)
ML_1 (pango_layout_get_spacing, PangoLayout_val, Val_int)
ML_2 (pango_layout_set_justify, PangoLayout_val, Bool_val, Unit)
ML_1 (pango_layout_get_justify, PangoLayout_val, Val_bool)
ML_2 (pango_layout_set_single_paragraph_mode, PangoLayout_val, Bool_val, Unit)
ML_1 (pango_layout_get_single_paragraph_mode, PangoLayout_val, Val_bool)
ML_1 (pango_layout_context_changed, PangoLayout_val, Unit)
CAMLprim value ml_pango_layout_get_size(value layout)
{
  int width, height;
  value res = alloc_tuple(2);
  pango_layout_get_size(PangoLayout_val(layout), &width, &height);
  Field(res,0) = Val_int(width);
  Field(res,1) = Val_int(height);
  return res;
}
CAMLprim value ml_pango_layout_get_pixel_size(value layout)
{
  int width, height;
  value res = alloc_tuple(2);
  pango_layout_get_pixel_size(PangoLayout_val(layout), &width, &height);
  Field(res,0) = Val_int(width);
  Field(res,1) = Val_int(height);
  return res;
}
CAMLexport value Val_PangoRectangle(PangoRectangle *rect)
{
  value res = alloc_tuple(4);
  Field(res,0) = Val_int(rect->x); Field(res,1) = Val_int(rect->y);
  Field(res,2) = Val_int(rect->width); Field(res,3) = Val_int(rect->height);
  return res;
}
CAMLprim value ml_pango_layout_index_to_pos(value layout, value index)
{
  PangoRectangle pos;
  pango_layout_index_to_pos(PangoLayout_val(layout), Int_val(index), &pos);
  return Val_PangoRectangle(&pos);
}
CAMLprim value ml_pango_layout_xy_to_index(value layout, value x, value y)
{
  int index, trailing;
  gboolean exact;
  value res;
  exact = pango_layout_xy_to_index(PangoLayout_val(layout), Int_val(x),
                                   Int_val(y), &index, &trailing);
  res = alloc_tuple(3);
  Field(res,0) = Val_int(index);
  Field(res,1) = Val_int(trailing);
  Field(res,2) = Val_bool(exact);
  return res;
}
CAMLprim value ml_pango_layout_get_extent(value layout)
{
  PangoRectangle ink;
  pango_layout_get_extents(PangoLayout_val(layout), &ink, NULL);
  return Val_PangoRectangle(&ink);
}
CAMLprim value ml_pango_layout_get_pixel_extent(value layout)
{
  PangoRectangle ink;
  pango_layout_get_pixel_extents(PangoLayout_val(layout), &ink, NULL);
  return Val_PangoRectangle(&ink);
}
#ifdef HASGTK26
ML_1(pango_layout_get_ellipsize, PangoLayout_val, Val_pango_ellipsize_mode)
ML_2(pango_layout_set_ellipsize, PangoLayout_val, Pango_ellipsize_mode_val, Unit)
#else
Unsupported_26(pango_layout_get_ellipsize)
Unsupported_26(pango_layout_set_ellipsize)
#endif /* HASGTK26 */
