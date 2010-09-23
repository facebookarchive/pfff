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

/* $Id: ml_gdk.c 1452 2009-05-08 10:15:38Z garrigue $ */

#include <string.h>
#include <gdk/gdk.h>
#if defined(_WIN32) || defined(__MINGW32__)
#include <gdk/gdkwin32.h>
#else
#if defined(HAS_GTKQUARTZ)
#else
#include <gdk/gdkx.h>
#endif
#endif
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include "wrappers.h"
#include "ml_gpointer.h"
#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_pango.h"
#include "ml_gdk.h"
#include "ml_gdkpixbuf.h"
#include "gdk_tags.h"


#ifndef HASGTK22
#define GDK_WINDOW_TYPE_HINT_SPLASHSCREEN GDK_WINDOW_TYPE_HINT_NORMAL
#define GDK_WINDOW_TYPE_HINT_DESKTOP GDK_WINDOW_TYPE_HINT_NORMAL
#define GDK_WINDOW_TYPE_HINT_UTILITY GDK_WINDOW_TYPE_HINT_NORMAL
#define GDK_WINDOW_TYPE_HINT_DOCK GDK_WINDOW_TYPE_HINT_NORMAL
#endif

CAMLprim void ml_raise_gdk (const char *errmsg)
{
  static value * exn = NULL;
  if (exn == NULL)
      exn = caml_named_value ("gdkerror");
  raise_with_string (*exn, (char*)errmsg);
}

CAMLprim value ml_gdk_init(value unit)
{
  /* Since these are declared const, must force gcc to call them! */
  GType t =
    gdk_color_get_type();
  return Val_GType(t);
}

#include "gdk_tags.c"

Make_OptFlags_val (GdkModifier_val)
Make_Flags_val (GdkModifier_val)
Make_Flags_val (Event_mask_val)

#include <stdio.h>

#define Make_test(conv) \
CAMLprim value ml_test_##conv (value mask, value test) \
{ return Val_bool (conv(mask) & Int_val(test)); }

Make_test(GdkModifier_val)
Make_test(GdkWindowState_val)

/* Platform */

value ml_gdk_get_platform()
{
#ifdef GDK_WINDOWING_WIN32
  return MLTAG_WIN32;
#elif defined(GDK_WINDOWING_QUARTZ)
  return MLTAG_QUARTZ;
#else
  return MLTAG_X11;
#endif
}

/* Colormap */

ML_0 (gdk_colormap_get_system, Val_GdkColormap)

/* Screen geometry */

ML_0 (gdk_screen_width, Val_int)
ML_0 (gdk_screen_height, Val_int)
ML_0 (gdk_pango_context_get, Val_PangoContext_new)

#ifdef GDK_SCREEN
ML_1 (gdk_screen_get_width, GdkScreen_val, Val_int)
ML_1 (gdk_screen_get_height, GdkScreen_val, Val_int)
ML_0 (gdk_screen_get_default, Val_GdkScreen)
ML_1 (gdk_pango_context_get_for_screen, GdkScreen_val, Val_PangoContext_new)
#else
Unsupported (gdk_screen_get_width)
Unsupported (gdk_screen_get_height)
Unsupported (gdk_screen_get_default)
Unsupported (gdk_pango_context_get_for_screen)
#endif

/* Visual */
CAMLprim value ml_gdk_visual_get_best (value depth, value type)
{
     GdkVisual *vis;
     if (type == Val_unit)
          if (depth == Val_unit) vis = gdk_visual_get_best ();
          else vis = gdk_visual_get_best_with_depth (Int_val(Field(depth,0)));
     else
          if (depth == Val_unit)
               vis = gdk_visual_get_best_with_type
                    (GdkVisualType_val(Field(type,0)));
          else vis = gdk_visual_get_best_with_both
                    (Int_val(Field(depth,0)),GdkVisualType_val(Field(type,0)));
     if (!vis) ml_raise_gdk("Gdk.Visual.get_best");
     return Val_GdkVisual(vis);
}

Make_Extractor (GdkVisual,GdkVisual_val,type,Val_gdkVisualType)
Make_Extractor (GdkVisual,GdkVisual_val,depth,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,red_mask,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,red_shift,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,red_prec,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,green_mask,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,green_shift,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,green_prec,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,blue_mask,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,blue_shift,Val_int)
Make_Extractor (GdkVisual,GdkVisual_val,blue_prec,Val_int)

/* Image */

#ifndef UnsafeImage
CAMLexport GdkImage *GdkImage_val(value val)
{
    if (!Field(val,1)) ml_raise_gdk ("attempt to use destroyed GdkImage");
    return check_cast(GDK_IMAGE,val);
}
#endif

/* Broken in 2.0
ML_4 (gdk_image_new_bitmap, GdkVisual_val, String_val, Int_val, Int_val,
      Val_GdkImage)
*/
ML_4 (gdk_image_new, GdkImageType_val, GdkVisual_val, Int_val, Int_val,
      Val_GdkImage_new)
ML_5 (gdk_drawable_get_image, GdkDrawable_val, Int_val, Int_val, Int_val,
      Int_val, Val_GdkImage_new)
ML_4 (gdk_image_put_pixel, GdkImage_val, Int_val, Int_val, Int_val, Unit)
ML_3 (gdk_image_get_pixel, GdkImage_val, Int_val, Int_val, Val_int)
Make_Extractor(gdk_image, GdkImage_val, visual, Val_GdkVisual)
Make_Extractor(gdk_image, GdkImage_val, width, Val_int)
Make_Extractor(gdk_image, GdkImage_val, height, Val_int)
Make_Extractor(gdk_image, GdkImage_val, depth, Val_int)

/*
Make_Extractor(gdk_image, GdkImage_val, bpp, Val_int)
Make_Extractor(gdk_image, GdkImage_val, bpl, Val_int)
Make_Extractor(gdk_image, GdkImage_val, mem, Val_pointer)
*/

/* Color */

ML_2 (gdk_colormap_new, GdkVisual_val, Bool_val, Val_GdkColormap)
ML_1 (gdk_colormap_get_visual, GdkColormap_val, Val_GdkVisual)

CAMLprim value ml_gdk_color_white (value cmap)
{
    GdkColor color;
    gdk_color_white (GdkColormap_val(cmap), &color);
    return Val_copy(color);
}
    
CAMLprim value ml_gdk_color_black (value cmap)
{
    GdkColor color;
    gdk_color_black (GdkColormap_val(cmap), &color);
    return Val_copy(color);
}

CAMLprim value ml_gdk_color_parse (char *spec)
{
    GdkColor color;
    if (!gdk_color_parse (spec, &color))
        ml_raise_gdk ("color_parse");
    return Val_copy(color);
}

ML_2 (gdk_color_alloc, GdkColormap_val, GdkColor_val, Val_bool)

CAMLprim value ml_GdkColor (value red, value green, value blue)
{
    GdkColor color;
    color.red = Int_val(red);
    color.green = Int_val(green);
    color.blue = Int_val(blue);
    color.pixel = 0;
    return Val_copy(color);
}

Make_Extractor (GdkColor, GdkColor_val, red, Val_int)
Make_Extractor (GdkColor, GdkColor_val, green, Val_int)
Make_Extractor (GdkColor, GdkColor_val, blue, Val_int)
Make_Extractor (GdkColor, GdkColor_val, pixel, Val_int)

/* Rectangle */

CAMLprim value ml_GdkRectangle (value x, value y, value width, value height)
{
    GdkRectangle rectangle;
    rectangle.x = Int_val(x);
    rectangle.y = Int_val(y);
    rectangle.width = Int_val(width);
    rectangle.height = Int_val(height);
    return Val_copy(rectangle);
}

Make_Extractor (GdkRectangle, GdkRectangle_val, x, Val_int)
Make_Extractor (GdkRectangle, GdkRectangle_val, y, Val_int)
Make_Extractor (GdkRectangle, GdkRectangle_val, width, Val_int)
Make_Extractor (GdkRectangle, GdkRectangle_val, height, Val_int)

/* Drawable */

ML_1 (gdk_drawable_get_visual, GdkDrawable_val, Val_GdkVisual)
ML_1 (gdk_drawable_get_colormap, GdkDrawable_val, Val_GdkColormap)
ML_1 (gdk_drawable_get_depth, GdkDrawable_val, Val_int)

CAMLprim value ml_gdk_drawable_get_size (value drawable)
{
  int x, y;
  value ret;

  gdk_drawable_get_size (GdkDrawable_val(drawable), &x, &y);
  
  ret = alloc_small (2,0);
  Field(ret,0) = Val_int(x);
  Field(ret,1) = Val_int(y);
  return ret;
}

/* Window */

ML_3 (gdk_window_set_back_pixmap, GdkWindow_val, GdkPixmap_val, Int_val, Unit)
ML_2 (gdk_window_set_cursor, GdkWindow_val, GdkCursor_val, Unit)
ML_1 (gdk_window_clear, GdkWindow_val, Unit)
ML_0 (GDK_ROOT_PARENT, Val_GdkWindow)
ML_1 (gdk_window_get_parent, GdkWindow_val, Val_GdkWindow)

#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAS_GTKQUARTZ)
CAMLprim value ml_GDK_WINDOW_XWINDOW(value v)
{
 ml_raise_gdk ("Not available for Win32");
 return Val_unit;
} 

#else
ML_1 (GDK_WINDOW_XWINDOW, GdkDrawable_val, Val_XID)
#endif
CAMLprim value ml_gdk_window_get_position (value window)
{
  int x, y;
  value ret;

  gdk_window_get_position (GdkWindow_val(window), &x, &y);
  
  ret = alloc_small (2,0);
  Field(ret,0) = Val_int(x);
  Field(ret,1) = Val_int(y);
  return ret;
}

CAMLprim value ml_gdk_window_get_pointer_location (value window)
{
  int x = 0;
  int y = 0;
  value ret;
  gdk_window_get_pointer (GdkWindow_val(window), &x, &y, NULL);
  ret = alloc_small (2, 0);
  Field(ret, 0) = Val_int(x);
  Field(ret, 1) = Val_int(y);
  return ret;
}

/* Cursor */

Make_Val_final_pointer_ext (GdkCursor, _new, Ignore, gdk_cursor_unref, 20)
ML_1 (gdk_cursor_new, GdkCursorType_val, Val_GdkCursor_new)
ML_6 (gdk_cursor_new_from_pixmap, GdkPixmap_val, GdkPixmap_val,
      GdkColor_val, GdkColor_val, Int_val, Int_val, Val_GdkCursor_new)
ML_bc6 (ml_gdk_cursor_new_from_pixmap)
#ifdef HASGTK24
ML_3 (gdk_cursor_new_from_pixbuf, Insert(gdk_display_get_default ())
      GdkPixbuf_val, Int_val, Int_val, Val_GdkCursor_new)
#else
Unsupported_24(gdk_cursor_new_from_pixbuf)
#endif
#ifdef HASGTK28
ML_1 (gdk_cursor_get_image, GdkCursor_val, Val_GdkPixbuf_new)
#else
Unsupported_28(gdk_cursor_get_image)
#endif

/* Display */
#ifdef HASGTK22
ML_0 (gdk_display_get_default, Val_GdkDisplay)
CAMLprim value ml_gdk_display_get_window_at_pointer (value display)
{
  gint x;
  gint y;
  GdkWindow *gwin;

  if ((gwin = gdk_display_get_window_at_pointer
       (GdkDisplay_val (display), &x, &y)))
  { /* return Some */
    CAMLparam0 ();
    CAMLlocal1(tup);

    tup = alloc_tuple(3);
    Store_field(tup,0,Val_GdkWindow(gwin));
    Store_field(tup,1,Val_int(x));
    Store_field(tup,2,Val_int(y));
    CAMLreturn(ml_some (tup));
  }
  return Val_unit;
}
#else
Unsupported_22(gdk_display_get_default)
Unsupported_22(gdk_display_get_window_at_pointer)
#endif

/* Pixmap */

CAMLexport GdkPixmap *GdkPixmap_val(value val)
{
    if (!Field(val,1)) ml_raise_gdk ("attempt to use destroyed GdkPixmap");
    return check_cast(GDK_PIXMAP,val);
}

ML_4 (gdk_pixmap_new, GdkWindow_val, Int_val, Int_val, Int_val,
      Val_GdkPixmap_no_ref)
ML_4 (gdk_bitmap_create_from_data, GdkWindow_val,
      String_val, Int_val, Int_val, Val_GdkBitmap_no_ref)
ML_7 (gdk_pixmap_create_from_data, GdkWindow_val, String_val,
      Int_val, Int_val, Int_val, GdkColor_val, GdkColor_val,
      Val_GdkPixmap_no_ref)
ML_bc7 (ml_gdk_pixmap_create_from_data)

CAMLprim value ml_gdk_pixmap_colormap_create_from_xpm
	(value window, value colormap, value transparent, char *filename)
{
    CAMLparam0();
    GdkPixmap *pixmap;
    GdkBitmap *mask = NULL;
    CAMLlocal2(vpixmap, vmask);
    value ret;

    pixmap = gdk_pixmap_colormap_create_from_xpm
        (Option_val(window,GdkWindow_val,NULL),
         Option_val(colormap,GdkColormap_val,NULL),
         &mask, Option_val(transparent,GdkColor_val,NULL), filename);
    if (!pixmap) ml_raise_gdk ("Gdk.Pixmap.create_from_xpm_file");
    vpixmap = Val_GdkPixmap_no_ref(pixmap);
    vmask = Val_GdkBitmap_no_ref (mask);

    ret = alloc_small (2,0);
    Field(ret,0) = vpixmap;
    Field(ret,1) = vmask;
    CAMLreturn(ret);
}

CAMLprim value ml_gdk_pixmap_colormap_create_from_xpm_d
	(value window, value colormap, value transparent, char **data)
{
    CAMLparam0();
    GdkPixmap *pixmap;
    GdkBitmap *mask = NULL;
    CAMLlocal2(vpixmap, vmask);
    value ret;

    pixmap = gdk_pixmap_colormap_create_from_xpm_d
        (Option_val(window,GdkWindow_val,NULL),
         Option_val(colormap,GdkColormap_val,NULL),
         &mask, Option_val(transparent,GdkColor_val,NULL), data);
    if (!pixmap) ml_raise_gdk ("Gdk.Pixmap.create_from_xpm_data");
    vpixmap = Val_GdkPixmap_no_ref (pixmap);
    vmask = Val_GdkBitmap_no_ref (mask);

    ret = alloc_small (2, 0);
    Field(ret,0) = vpixmap;
    Field(ret,1) = vmask;
    CAMLreturn(ret);
}

/* Font */

Make_Val_final_pointer (GdkFont, gdk_font_ref, gdk_font_unref, 0)
Make_Val_final_pointer_ext (GdkFont, _no_ref, Ignore, gdk_font_unref, 20)
ML_1 (gdk_font_load, String_val, Val_GdkFont_no_ref)
ML_1 (gdk_fontset_load, String_val, Val_GdkFont_no_ref)
ML_2 (gdk_string_width, GdkFont_val, String_val, Val_int)
ML_2 (gdk_char_width, GdkFont_val, (gchar)Long_val, Val_int)
ML_2 (gdk_string_height, GdkFont_val, String_val, Val_int)
ML_2 (gdk_char_height, GdkFont_val, (gchar)Long_val, Val_int)
ML_2 (gdk_string_measure, GdkFont_val, String_val, Val_int)
ML_2 (gdk_char_measure, GdkFont_val, (char)Long_val, Val_int)
Make_Extractor (GdkFont, GdkFont_val, type, Val_font_type)
Make_Extractor (GdkFont, GdkFont_val, ascent, Val_int)
Make_Extractor (GdkFont, GdkFont_val, descent, Val_int)

/* Properties */

ML_2 (gdk_atom_intern, String_val, Int_val, Val_GdkAtom)
ML_1 (gdk_atom_name, GdkAtom_val, Val_string)

CAMLprim value ml_gdk_property_change (value window, value property, value type,
                              value mode, value xdata)
{
    int format = Xdata_val (Field(xdata,0));
    value data = Field(xdata,1);
    int nelems = (format == 8 ? string_length (data) : Wosize_val(data));
    guchar *sdata;
    int i;
    switch (format) {
    case 16:
        sdata = calloc(nelems, sizeof(short)); 
        for (i=0; i<nelems; i++)
            ((gushort*)sdata)[i] = Int_val(Field(data,i));
        break;
    case 32:
        sdata = calloc(nelems, sizeof(long));
        for (i=0; i<nelems; i++)
            ((gulong*)sdata)[i] = Int32_val(Field(data,i)); 
        break;
    default:
        sdata = (guchar*)data;
    }
    gdk_property_change (GdkWindow_val(window), GdkAtom_val(property),
                         GdkAtom_val(type), format, Property_mode_val(mode),
                         sdata, nelems);
    if (format != 8) free(sdata);
    return Val_unit;
}

/* copy X11 property data */
CAMLprim value copy_xdata (gint format, void *xdata, gulong nitems)
{
    CAMLparam0();
    CAMLlocal1(data);
    value ret = MLTAG_NONE;
    value tag;
    unsigned int i;
    switch (format) {
    case 8:
        data = alloc_string (nitems);
        memcpy (String_val(data), xdata, sizeof(char) * nitems);
        tag = MLTAG_BYTES;
        break;
    case 16:
        data = alloc (nitems,0);
        for (i = 0; i < nitems; i++)
            Field(data,i) = Val_int(((short*)xdata)[i]);
        tag = MLTAG_SHORTS;
        break;
    case 32:
        data = alloc (nitems,0);
        for (i = 0; i < nitems; i++)
            Store_field(data, i, copy_int32 (((long*)xdata)[i]));
        tag = MLTAG_INT32S;
        break;
    default:
        tag = MLTAG_NONE;
    }
    if (tag != MLTAG_NONE) {
        ret = alloc_small (2,0);
        Field(ret,0) = tag;
        Field(ret,1) = data;
    }
    CAMLreturn(ret);
}

CAMLprim value ml_gdk_property_get (value window, value property,
                           value length, value pdelete)
{
#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAS_GTKQUARTZ)
  return Val_unit; /* not supported */
#else
    GdkAtom atype;
    int aformat, alength;
    guchar *data;
    int nitems;
    int ok = gdk_property_get (GdkWindow_val(window), GdkAtom_val(property),
                               AnyPropertyType, 0,
                               Long_val(length), Bool_val(pdelete),
                               &atype, &aformat, &alength, &data);
    if (ok) {
        CAMLparam0();
        CAMLlocal3(mltype, mldata, pair);
        switch (aformat) {
        case 16: nitems = alength / sizeof(short); break;
        case 32: nitems = alength / sizeof(long); break;
        default: nitems = alength;
        }
        mldata = copy_xdata (aformat, data, nitems);
        mltype = Val_GdkAtom (atype);
        pair = alloc_small(2,0);
        Field(pair,0) = mltype;
        Field(pair,1) = mldata;
        CAMLreturn(ml_some (pair));
    }
    return Val_unit;
#endif
}

ML_2 (gdk_property_delete, GdkWindow_val, GdkAtom_val, Unit)

/* Region */

#define PointArray_val(val) ((GdkPoint*)&Field(val,1))
#define PointArrayLen_val(val) Int_val(Field(val,0))
Make_Val_final_pointer (GdkRegion, Ignore, gdk_region_destroy, 0)
#define Val_GdkRegion_copy(r) (Val_GdkRegion(gdk_region_copy(r)))
CAMLexport GdkRegion *GdkRegion_val(value val)
{
    if (!Field(val,1)) ml_raise_gdk ("attempt to use destroyed GdkRegion");
    return (GdkRegion*)(Field(val,1));
}
CAMLprim value ml_gdk_region_destroy (value val)
{
    if (Field(val,1)) gdk_region_destroy((GdkRegion*)(Field(val,1)));
    Field(val,1) = 0;
    return Val_unit;
}
ML_0 (gdk_region_new, Val_GdkRegion)
ML_2 (gdk_region_polygon, Insert(PointArray_val(arg1)) PointArrayLen_val,
      Fill_rule_val, Val_GdkRegion)
ML_1 (gdk_region_copy, GdkRegion_val, Val_GdkRegion)
ML_2 (gdk_region_intersect, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_union, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_subtract, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_xor, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_union_with_rect, GdkRegion_val, GdkRectangle_val, Unit)
ML_3 (gdk_region_offset, GdkRegion_val, Int_val, Int_val, Unit)
ML_3 (gdk_region_shrink, GdkRegion_val, Int_val, Int_val, Unit)
ML_1 (gdk_region_empty, GdkRegion_val, Val_bool)
ML_2 (gdk_region_equal, GdkRegion_val, GdkRegion_val, Val_bool)
ML_3 (gdk_region_point_in, GdkRegion_val, Int_val, Int_val, Val_bool)
ML_2 (gdk_region_rect_in, GdkRegion_val, GdkRectangle_val, Val_overlap_type)
ML_2 (gdk_region_get_clipbox, GdkRegion_val, GdkRectangle_val, Unit)


/* GC */

ML_1 (gdk_gc_new, GdkDrawable_val, Val_GdkGC_no_ref)
ML_2 (gdk_gc_set_foreground, GdkGC_val, GdkColor_val, Unit)
ML_2 (gdk_gc_set_background, GdkGC_val, GdkColor_val, Unit)
ML_2 (gdk_gc_set_font, GdkGC_val, GdkFont_val, Unit)
ML_2 (gdk_gc_set_function, GdkGC_val, Function_type_val, Unit)
ML_2 (gdk_gc_set_fill, GdkGC_val, Fill_val, Unit)
ML_2 (gdk_gc_set_tile, GdkGC_val, GdkPixmap_val, Unit)
ML_2 (gdk_gc_set_stipple, GdkGC_val, GdkPixmap_val, Unit)
ML_3 (gdk_gc_set_ts_origin, GdkGC_val, Int_val, Int_val, Unit)
ML_3 (gdk_gc_set_clip_origin, GdkGC_val, Int_val, Int_val, Unit)
ML_2 (gdk_gc_set_clip_mask, GdkGC_val, GdkBitmap_val, Unit)
ML_2 (gdk_gc_set_clip_rectangle, GdkGC_val, GdkRectangle_val, Unit)
ML_2 (gdk_gc_set_clip_region, GdkGC_val, GdkRegion_val, Unit)
ML_2 (gdk_gc_set_subwindow, GdkGC_val, Subwindow_mode_val, Unit)
ML_2 (gdk_gc_set_exposures, GdkGC_val, Bool_val, Unit)
ML_5 (gdk_gc_set_line_attributes, GdkGC_val, Int_val, Line_style_val,
      Cap_style_val, Join_style_val, Unit)

CAMLprim value ml_gdk_gc_set_dashes(value gc, value offset, value dashes)
{
  CAMLparam3(gc, offset, dashes);
  CAMLlocal1(tmp);
  int l = 0;
  int i;
  gint8 *cdashes;
  for(tmp = dashes; tmp != Val_int(0); tmp = Field(tmp,1)){
    l++;
  }
  if( l == 0 ){ ml_raise_gdk("line dashes must have at least one element"); }
  cdashes = stat_alloc(sizeof (gint8) * l);
  for(i=0, tmp= dashes; i<l; i++, tmp = Field(tmp,1)){
    int d;
    d = Int_val(Field(tmp,0));
    if( d<0 || d>255 ){
      stat_free (cdashes);
      ml_raise_gdk("line dashes must be [0..255]");
    }
    cdashes[i] = d;
  }
  gdk_gc_set_dashes( GdkGC_val(gc), Int_val(offset), cdashes, l);
  /* stat_free (cdashes); ? */
  CAMLreturn(Val_unit);
}
  

ML_2 (gdk_gc_copy, GdkGC_val, GdkGC_val, Unit)
CAMLprim value ml_gdk_gc_get_values (value gc)
{
    CAMLparam0();
    GdkGCValues values;
    CAMLlocal2(ret, tmp);

    gdk_gc_get_values (GdkGC_val(gc), &values);
    ret = alloc (18, 0);
    tmp = Val_copy(values.foreground); Store_field(ret, 0, tmp);
    tmp = Val_copy(values.background); Store_field(ret, 1, tmp);
    if (values.font) {
        tmp = ml_some(Val_GdkFont(values.font));
        Store_field(ret, 2, tmp);
    }
    else
      Store_field(ret, 2, Val_int(0));
    Field(ret,3) = Val_function_type(values.function);
    Field(ret,4) = Val_fill(values.fill);
    if (values.tile) {
        tmp = ml_some(Val_GdkPixmap(values.tile));
        Store_field(ret, 5, tmp);
    }
    else
      Store_field(ret, 5, Val_int(0));
    if (values.stipple) {
        tmp = ml_some(Val_GdkPixmap(values.stipple));
        Store_field(ret, 6, tmp);
    }
    else
      Store_field(ret, 6, Val_int(0));
    if (values.clip_mask) {
        tmp = ml_some(Val_GdkPixmap(values.clip_mask));
        Store_field(ret, 7, tmp);
    }
    else
      Store_field(ret, 7, Val_int(0));
    Field(ret,8) = Val_subwindow_mode(values.subwindow_mode);
    Field(ret,9) = Val_int(values.ts_x_origin);
    Field(ret,10) = Val_int(values.ts_y_origin);
    Field(ret,11) = Val_int(values.clip_x_origin);
    Field(ret,12) = Val_int(values.clip_y_origin);
    Field(ret,13) = Val_bool(values.graphics_exposures);
    Field(ret,14) = Val_int(values.line_width);
    Field(ret,15) = Val_line_style(values.line_style);
    Field(ret,16) = Val_cap_style(values.cap_style);
    Field(ret,17) = Val_join_style(values.join_style);
    CAMLreturn(ret);
}

/* Draw */

CAMLprim value ml_point_array_new (value len)
{
  value ret;
    if(Int_val(len) <= 0)
      invalid_argument("PointArray.new");
    ret = alloc (1 + Wosize_asize(Int_val(len)*sizeof(GdkPoint)),
		 Abstract_tag);
    Field(ret,0) = len;
    return ret;
}
CAMLprim value ml_point_array_set (value arr, value pos, value x, value y)
{
    GdkPoint *pt = PointArray_val(arr) + Int_val(pos);
    pt->x = Int_val(x);
    pt->y = Int_val(y);
    return Val_unit;
}

#define SegmentArray_val(val) ((GdkSegment*)&Field(val,1))
#define SegmentArrayLen_val(val) Int_val(Field(val,0))
CAMLprim value ml_segment_array_new (value len)
{
    value ret;
    if(Int_val(len) <= 0)
      invalid_argument("SegmentArray.new");
    ret = alloc (1 + Wosize_asize(Int_val(len)*sizeof(GdkSegment)),
		 Abstract_tag);
    Field(ret,0) = len;
    return ret;
}
CAMLprim value ml_segment_array_set (value arr, value pos, value x1, value y1, value x2, value y2)
{
    GdkSegment *pt = SegmentArray_val(arr) + Int_val(pos);
    pt->x1 = Int_val(x1);
    pt->y1 = Int_val(y1);
    pt->x2 = Int_val(x2);
    pt->y2 = Int_val(y2);
    return Val_unit;
}
ML_bc6 (ml_segment_array_set)

ML_4 (gdk_draw_point, GdkDrawable_val, GdkGC_val, Int_val, Int_val, Unit)
ML_6 (gdk_draw_line, GdkDrawable_val, GdkGC_val, Int_val, Int_val,
      Int_val, Int_val, Unit)
ML_bc6 (ml_gdk_draw_line)
ML_7 (gdk_draw_rectangle, GdkDrawable_val, GdkGC_val, Bool_val,
      Int_val, Int_val, Int_val, Int_val, Unit)
ML_bc7 (ml_gdk_draw_rectangle)
ML_9 (gdk_draw_arc, GdkDrawable_val, GdkGC_val, Bool_val, Int_val, Int_val,
      Int_val, Int_val, Int_val, Int_val, Unit)
ML_bc9 (ml_gdk_draw_arc)
ML_4 (gdk_draw_polygon, GdkDrawable_val, GdkGC_val, Bool_val,
      Insert(PointArray_val(arg4)) PointArrayLen_val, Unit)
ML_6 (gdk_draw_string, GdkDrawable_val, GdkFont_val, GdkGC_val, Int_val,
      Int_val, String_val, Unit)
ML_bc6 (ml_gdk_draw_string)
ML_7 (gdk_draw_layout_with_colors, GdkDrawable_val, GdkGC_val, Int_val,
      Int_val, PangoLayout_val, Option_val(arg6,GdkColor_val,NULL) Ignore,
      Option_val(arg7,GdkColor_val,NULL) Ignore, Unit)
ML_bc7 (ml_gdk_draw_layout_with_colors)

/*
ML_9 (gdk_draw_bitmap, GdkDrawable_val, GdkGC_val, GdkBitmap_val, Int_val, Int_val, Int_val, Int_val, Int_val, Int_val, Unit)
ML_bc9 (ml_gdk_draw_bitmap)
*/
ML_9 (gdk_draw_pixmap, GdkDrawable_val, GdkGC_val, GdkPixmap_val, Int_val, Int_val, Int_val, Int_val, Int_val, Int_val, Unit)
ML_bc9 (ml_gdk_draw_pixmap)
ML_9 (gdk_draw_image, GdkDrawable_val, GdkGC_val, GdkImage_val, Int_val, Int_val, Int_val, Int_val, Int_val, Int_val, Unit)
ML_bc9 (ml_gdk_draw_image)
ML_3 (gdk_draw_points, GdkDrawable_val, GdkGC_val, 
      Insert(PointArray_val(arg3)) PointArrayLen_val, Unit)
ML_3 (gdk_draw_segments, GdkDrawable_val, GdkGC_val, 
      Insert(SegmentArray_val(arg3)) SegmentArrayLen_val, Unit)
ML_3 (gdk_draw_lines, GdkDrawable_val, GdkGC_val, 
      Insert(PointArray_val(arg3)) PointArrayLen_val, Unit)

/* RGB */

ML_0 (gdk_rgb_init, Unit)
ML_0 (gdk_rgb_get_visual, Val_GdkVisual)
ML_0 (gdk_rgb_get_cmap, Val_GdkColormap)
ML_9 (gdk_draw_rgb_image, GdkDrawable_val, GdkGC_val, Int_val, Int_val,
      Int_val, Int_val, GdkRgbDither_val, ml_gpointer_base, Int_val, Unit)
ML_bc9 (ml_gdk_draw_rgb_image)

/* Events */

/* Have a major collection every 1000 events */
Make_Val_final_pointer (GdkEvent, Ignore, gdk_event_free, 1)
ML_1 (gdk_event_copy, GdkEvent_val, Val_GdkEvent)

#ifdef HASGTK22
CAMLprim value ml_gdk_event_new (value event_type)
{
    GdkEvent *event = gdk_event_new(Event_type_val(event_type));
    event->any.send_event = TRUE;
    return Val_GdkEvent(event);
}
#else
CAMLprim value ml_gdk_event_new (value event_type)
{
    GdkEvent event;
    memset (&event, 0, sizeof(GdkEvent));
    event.type = Event_type_val(event_type);
    event.any.send_event = TRUE;
    return Val_copy(event);
}
#endif

ML_1 (gdk_event_get_time, GdkEvent_val, copy_int32)

#define GdkEvent_arg(type) (GdkEvent##type*)GdkEvent_val

Make_Extractor (GdkEventAny, GdkEvent_arg(Any), type, Val_event_type)
Make_Extractor (GdkEventAny, GdkEvent_arg(Any), window, Val_GdkWindow)
Make_Extractor (GdkEventAny, GdkEvent_arg(Any), send_event, Val_bool)
Make_Setter (gdk_event_set, GdkEvent_arg(Any), Event_type_val, type)
Make_Setter (gdk_event_set, GdkEvent_arg(Any), GdkWindow_val, window)

Make_Extractor (GdkEventExpose, GdkEvent_arg(Expose), area, Val_copy)
Make_Extractor (GdkEventExpose, GdkEvent_arg(Expose), region,
                Val_GdkRegion_copy)
Make_Extractor (GdkEventExpose, GdkEvent_arg(Expose), count, Val_int)

Make_Extractor (GdkEventVisibility, GdkEvent_arg(Visibility), state,
                Val_gdkVisibilityState)

Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), x, copy_double)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), y, copy_double)
static value copy_axes(double *axes)
{
    CAMLparam0();
    CAMLlocal2(x,y);
    value ret;
    if (axes) {
        x = copy_double(axes[0]);
        y = copy_double(axes[0]);
        ret = alloc_small(2, 0);
        Field(ret,0) = x;
        Field(ret,1) = y;
        ret = ml_some(ret);
    }
    else ret = Val_unit;
    CAMLreturn(ret);
}
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), axes, copy_axes)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), state, Val_int)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), is_hint, Val_int)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), device, Val_GdkDevice)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), x_root, copy_double)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), y_root, copy_double)

Make_Extractor (GdkEventButton, GdkEvent_arg(Button), x, copy_double)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), y, copy_double)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), axes, copy_axes)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), state, Val_int)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), button, Val_int)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), device, Val_GdkDevice)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), x_root, copy_double)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), y_root, copy_double)
Make_Setter (gdk_event_button_set, GdkEvent_arg(Button), Int_val, button)

Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), x, copy_double)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), y, copy_double)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), state, Val_int)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll),
                direction, Val_gdkScrollDirection)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), device, Val_GdkDevice)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), x_root, copy_double)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), y_root, copy_double)

Make_Extractor (GdkEventKey, GdkEvent_arg(Key), state, Val_int)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), keyval, Val_int)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), string, Val_string)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), hardware_keycode, Val_int)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), group, Val_int)

Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing),
                subwindow, Val_GdkWindow)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), x, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), y, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), x_root, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), y_root, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing),
                mode, Val_gdkCrossingMode)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing),
                detail, Val_gdkNotifyType)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), focus, Val_bool)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), state, Val_int)

Make_Extractor (GdkEventFocus, GdkEvent_arg(Focus), in, Val_int)

Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), x, Val_int)
Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), y, Val_int)
Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), width, Val_int)
Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), height, Val_int)

Make_Extractor (GdkEventProperty, GdkEvent_arg(Property), atom, Val_GdkAtom)
Make_Extractor (GdkEventProperty, GdkEvent_arg(Property), state, Val_int)

Make_Extractor (GdkEventSelection, GdkEvent_arg(Selection), selection,
                Val_GdkAtom)
Make_Extractor (GdkEventSelection, GdkEvent_arg(Selection), target,
                Val_GdkAtom)
Make_Extractor (GdkEventSelection, GdkEvent_arg(Selection), property,
                Val_GdkAtom)
Make_Extractor (GdkEventSelection, GdkEvent_arg(Selection), requestor, Val_XID)

Make_Extractor (GdkEventProximity, GdkEvent_arg(Proximity),
                device, Val_GdkDevice)

Make_Extractor (GdkEventClient, GdkEvent_arg(Client), window, Val_GdkWindow)
Make_Extractor(GdkEventClient, GdkEvent_arg(Client), message_type, Val_GdkAtom)
CAMLprim value ml_GdkEventClient_data (GdkEventClient *ev)
{
    int nitems = 0;
    switch (ev->data_format) {
    case 8:  nitems = 20; break;
    case 16: nitems = 10; break;
    case 32: nitems = 5;  break;
    }
    return copy_xdata (ev->data_format, ev->data.b, nitems);
}

Make_Extractor (GdkEventSetting, GdkEvent_arg(Setting),
                action, Val_gdkSettingAction)
Make_Extractor (GdkEventSetting, GdkEvent_arg(Setting), name, copy_string)

Make_Extractor (GdkEventWindowState, GdkEvent_arg(WindowState),
                changed_mask, Val_int)
Make_Extractor (GdkEventWindowState, GdkEvent_arg(WindowState),
                new_window_state, Val_int)

/* DnD */
Make_Flags_val (GdkDragAction_val)
#define GdkDragAction_optval(v) Option_val(v,GdkDragAction_val,0)
ML_3 (gdk_drag_status, GdkDragContext_val, GdkDragAction_optval, Int32_val,
      Unit)
Make_Extractor (GdkDragContext, GdkDragContext_val, suggested_action,
                Val_gdkDragAction)
static value val_int(gpointer i)
{
  return Val_int (GPOINTER_TO_INT(i));
}
CAMLprim value ml_GdkDragContext_targets (value c)
{
  GList *t;

  t = (GdkDragContext_val(c))->targets;
  return Val_GList (t, val_int);
}

/* Misc */
ML_0 (gdk_flush, Unit)
ML_0 (gdk_beep, Unit)
