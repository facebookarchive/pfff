/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/


#include "ml_cairo.h"

#if CAIRO_HAS_FT_FONT
# include <cairo-ft.h>

/* minimal Freetype interface */
static void
ml_raise_FT_Error (FT_Error err)
{
  static value *caml_exn;
  if (err == FT_Err_Ok)
    return;

  if (caml_exn == NULL)
    {
      caml_exn = caml_named_value ("FT_exn");
      if (caml_exn == NULL)
	caml_failwith ("freetype error");
    }

  caml_raise_with_arg (*caml_exn, Val_int (err));
}

static value
Val_ptr (void *p)
{
  value v = caml_alloc_small (1, Abstract_tag);
  Field (v, 0) = Val_bp (p);
  return v;
}

#define FT_Library_val(v) (FT_Library)(Field(v, 0))

CAMLprim value
ml_FT_Init_FreeType (value unit)
{
  FT_Library lib;
  ml_raise_FT_Error (FT_Init_FreeType (&lib));
  return Val_ptr (lib);
}

CAMLprim value
ml_FT_Done_FreeType (value lib)
{
  ml_raise_FT_Error (FT_Done_FreeType (FT_Library_val (lib)));
  return Val_unit;
}

#define FT_Face_val(v) (FT_Face)(Field(v, 0))

CAMLprim value
ml_FT_New_Face (value lib, value o_index, value path)
{
  FT_Face face;
  FT_Long index = Is_block(o_index) ? Long_val(Field(o_index, 0)) : 0;
  ml_raise_FT_Error (FT_New_Face (FT_Library_val (lib),
				  String_val (path),
				  index, &face));
  return Val_ptr (face);
}

CAMLprim value
ml_FT_Done_Face (value face)
{
  ml_raise_FT_Error (FT_Done_Face (FT_Face_val (face)));
  return Val_unit;
}

/* minimal Fontconfig interface */
wMake_Val_final_pointer (FcPattern, FcPatternDestroy, 10)
#define FcPattern_val(v) wPointer_val(FcPattern,v)

#define UString_val(v) ((unsigned char *) (v))

CAMLprim value
ml_FcNameParse (value fo, value s)
{
  FcPattern *p1, *p2;
  FcResult res;
  p1 = FcNameParse (UString_val(s));
  FcConfigSubstitute (NULL, p1, FcMatchPattern);
  if (Is_block (fo))
    {
      cairo_ft_font_options_substitute (cairo_font_options_t_val (Field (fo, 0)), p1);
    }
  FcDefaultSubstitute (p1);
  p2 = FcFontMatch (NULL, p1, &res);
  FcPatternDestroy (p1);
  return Val_FcPattern (p2);
}

CAMLprim value
ml_FcNameUnparse (value patt)
{
  FcChar8 *s;
  value r;
  s = FcNameUnparse (FcPattern_val (patt));
  if (s == NULL)
    caml_failwith ("FcNameUnparse");
  r = caml_copy_string ((char *) s);
  free (s);
  return r;
}

/* cairo Fontconfig/Freetype font backend */
wML_1 (cairo_ft_font_face_create_for_pattern, FcPattern_val, Val_cairo_font_face_t)
wML_2 (cairo_ft_font_face_create_for_ft_face, FT_Face_val, Int_val, Val_cairo_font_face_t)
wML_1 (cairo_ft_scaled_font_lock_face, cairo_scaled_font_t_val, Val_ptr)
wML_1 (cairo_ft_scaled_font_unlock_face, cairo_scaled_font_t_val, Unit)

#else

Cairo_Unsupported (FT_Init_FreeType, "FT backend not supported")
Cairo_Unsupported (FT_Done_FreeType, "FT backend not supported")
Cairo_Unsupported (FT_New_Face, "FT backend not supported")
Cairo_Unsupported (FT_Done_Face, "FT backend not supported")
Cairo_Unsupported (FcNameParse, "FT backend not supported")
Cairo_Unsupported (FcNameUnparse, "FT backend not supported")
Cairo_Unsupported (cairo_ft_font_create_for_pattern, "FT backend not supported")
Cairo_Unsupported (cairo_ft_font_create_for_ft_face, "FT backend not supported")
Cairo_Unsupported (cairo_ft_scaled_font_lock_face, "FT backend not supported")
Cairo_Unsupported (cairo_ft_scaled_font_unlock_face, "FT backend not supported")

#endif /* CAIRO_HAS_FT_FONT */
