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

/* $Id: ml_gnomecanvas.c 1347 2007-06-20 07:40:34Z guesdon $ */

#include <string.h>

#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>
#include <libart_lgpl/libart.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_gobject.h"
#include "ml_glib.h"
#include "ml_gdk.h"
#include "ml_gtk.h"

static inline value copy_two_doubles(double a, double b)
{
  CAMLparam0();
  CAMLlocal3(va, vb, v);
  va = copy_double(a);
  vb = copy_double(b);
  v = alloc_small(2, 0);
  Field(v, 0) = va;
  Field(v, 1) = vb;
  CAMLreturn(v);
}

static inline value copy_double_array(double *a, size_t len)
{
  value v;
  register unsigned int i;
  v = alloc(len * Double_wosize, Double_array_tag);
  for(i=0; i<len; i++)
    Store_double_field(v, i, a[i]);
  return v;
}


/* Types stuff */
CAMLprim value ml_gnome_canvas_register_types(value unit)
{
  value v = alloc_small(15, 0);
  Field(v,  0) = Val_GType(GNOME_TYPE_CANVAS);
  Field(v,  1) = Val_GType(GNOME_TYPE_CANVAS_BPATH);
  Field(v,  2) = Val_GType(GNOME_TYPE_CANVAS_WIDGET);
  Field(v,  3) = Val_GType(GNOME_TYPE_CANVAS_ELLIPSE);
  Field(v,  4) = Val_GType(GNOME_TYPE_CANVAS_GROUP);
  Field(v,  5) = Val_GType(GNOME_TYPE_CANVAS_ITEM);
  Field(v,  6) = Val_GType(GNOME_TYPE_CANVAS_LINE);
  Field(v,  7) = Val_GType(GNOME_TYPE_CANVAS_PIXBUF);
  Field(v,  8) = Val_GType(GNOME_TYPE_CANVAS_POINTS);
  Field(v,  9) = Val_GType(GNOME_TYPE_CANVAS_POLYGON);
  Field(v, 10) = Val_GType(GNOME_TYPE_CANVAS_RE);
  Field(v, 11) = Val_GType(GNOME_TYPE_CANVAS_RECT);
  Field(v, 12) = Val_GType(GNOME_TYPE_CANVAS_RICH_TEXT);
  Field(v, 13) = Val_GType(GNOME_TYPE_CANVAS_SHAPE);
  Field(v, 14) = Val_GType(GNOME_TYPE_CANVAS_TEXT);
  /* Field(v, 15) = Val_GType(GNOME_TYPE_CANVAS_CLIPGROUP); */
  return v;
}


/* GnomeCanvas */

#define GnomeCanvas_val(val)      check_cast(GNOME_CANVAS,val)
#define GnomeCanvasGroup_val(val) check_cast(GNOME_CANVAS_GROUP,val)
#define GnomeCanvasItem_val(val)  check_cast(GNOME_CANVAS_ITEM,val)

ML_0 (gnome_canvas_new, Val_GtkWidget_sink)
ML_0 (gnome_canvas_new_aa, Val_GtkWidget_sink)
ML_1 (gnome_canvas_root, GnomeCanvas_val, Val_GtkAny)
ML_5 (gnome_canvas_set_scroll_region, GnomeCanvas_val, Double_val, Double_val, Double_val, Double_val, Unit)

CAMLprim value ml_gnome_canvas_get_scroll_region(value c)
{
  double p[4];
  gnome_canvas_get_scroll_region(GnomeCanvas_val(c),  &p[0], &p[1], &p[2], &p[3]);
  return copy_double_array(p, 4);
}

ML_2 (gnome_canvas_set_center_scroll_region, GnomeCanvas_val, Bool_val, Unit)
ML_1 (gnome_canvas_get_center_scroll_region, GnomeCanvas_val, Val_bool)

ML_2 (gnome_canvas_set_pixels_per_unit, GnomeCanvas_val, Double_val, Unit)

ML_3 (gnome_canvas_scroll_to, GnomeCanvas_val, Int_val, Int_val, Unit)
CAMLprim value ml_gnome_canvas_get_scroll_offsets(value c)
{
  value v;
  int cx, cy;
  gnome_canvas_get_scroll_offsets(GnomeCanvas_val(c), &cx, &cy);
  v = alloc_small(2, 0);
  Field(v, 0) = Val_int(cx);
  Field(v, 1) = Val_int(cy);
  return v;
}

ML_1 (gnome_canvas_update_now, GnomeCanvas_val, Unit)

CAMLprim value ml_gnome_canvas_get_item_at(value c, value x, value y)
{
  GnomeCanvasItem *it = gnome_canvas_get_item_at(GnomeCanvas_val(c), Double_val(x), Double_val(y));
  if(! it)
    raise_not_found();
  return Val_GtkAny(it);
}

/* gnome_canvas_request_redraw_uta */
/* gnome_canvas_request_redraw */

#ifndef ARCH_ALIGN_DOUBLE
CAMLprim value ml_gnome_canvas_w2c_affine(value c)
{
  GnomeCanvas *canvas = GnomeCanvas_val(c);
  value v = alloc_small(6 * Double_wosize, Double_array_tag);
  gnome_canvas_w2c_affine(canvas, (double *)v);
  return v;
}
#else
CAMLprim value ml_gnome_canvas_w2c_affine(value c)
{
  GnomeCanvas *canvas = GnomeCanvas_val(c);
  value v = alloc_small(6 * Double_wosize, Double_array_tag);
  double coords[6];
  gnome_canvas_w2c_affine(canvas, coords);
  memcpy(Bp_val(v), coords, sizeof coords);
  return v;
}
#endif /* ARCH_ALIGN_DOUBLE */

CAMLprim value ml_gnome_canvas_w2c(value c, value wx, value wy)
{
  value v;
  int cx, cy;
  gnome_canvas_w2c(GnomeCanvas_val(c), Double_val(wx), Double_val(wy), &cx, &cy);
  v = alloc_small(2, 0);
  Field(v, 0) = Val_int(cx);
  Field(v, 1) = Val_int(cy);
  return v;
}

CAMLprim value ml_gnome_canvas_w2c_d(value c, value wx, value wy)
{
  double cx, cy;
  gnome_canvas_w2c_d(GnomeCanvas_val(c), Double_val(wx), Double_val(wy), &cx, &cy);
  return copy_two_doubles(cx, cy);
}

CAMLprim value ml_gnome_canvas_c2w(value c, value cx, value cy)
{
  double wx, wy;
  gnome_canvas_w2c_d(GnomeCanvas_val(c), Int_val(cx), Int_val(cy), &wx, &wy);
  return copy_two_doubles(wx, wy);
}

CAMLprim value ml_gnome_canvas_window_to_world(value c, value winx, value winy)
{
  double wox, woy;
  gnome_canvas_window_to_world(GnomeCanvas_val(c), Double_val(winx), Double_val(winy), &wox, &woy);
  return copy_two_doubles(wox, woy);
}

CAMLprim value ml_gnome_canvas_world_to_window(value c, value wox, value woy)
{
  double winx, winy;
  gnome_canvas_world_to_window(GnomeCanvas_val(c), Double_val(wox), Double_val(woy), &winx, &winy);
  return copy_two_doubles(winx, winy);
}

/* gnome_canvas_get_color */
/* gnome_canvas_get_color_pixel */
/* gnome_canvas_set_stipple_origin */
/* gnome_canvas_set_dither */
/* gnome_canvas_get_dither */


/* GnomeCanvasItem */

CAMLprim value ml_gnome_canvas_item_new(value p, value type)
{
  return Val_GtkAny_sink(gnome_canvas_item_new(GnomeCanvasGroup_val(p), GType_val(type), NULL));
}

Make_Extractor (gnome_canvas_item, GnomeCanvasItem_val, parent, Val_GtkAny)
Make_Extractor (gnome_canvas_item, GnomeCanvasItem_val, canvas, Val_GtkAny)

#define ML_TAG_AFFINE Val_int(-105142369)
#define ML_TAG_TRANSL Val_int(-133679292)
#define ML_TAG_IDENTITY Val_int(313840958)

CAMLprim value ml_gnome_canvas_item_xform(value i)
{
  GnomeCanvasItem *item = GnomeCanvasItem_val(i);
  if (item->xform) {
    CAMLparam0();
    CAMLlocal2(arr, v);
    mlsize_t len;
    if( GTK_OBJECT_FLAGS(GTK_OBJECT(item)) & GNOME_CANVAS_ITEM_AFFINE_FULL )
      len = 6;
    else
      len = 2;
    arr = alloc_small(Double_wosize * len, Double_array_tag);
    memcpy(Bp_val(arr), item->xform, len * sizeof (double));
    v = alloc_small(2, 0);
    Field(v, 0) = (len == 6) ? ML_TAG_AFFINE : ML_TAG_TRANSL;
    Field(v, 1) = arr;
    CAMLreturn(v);
  }
  else
    return ML_TAG_IDENTITY;
}

#ifndef ARCH_ALIGN_DOUBLE
CAMLprim value ml_gnome_canvas_item_affine_relative(value i, value a)
{
  if(Wosize_val(a) != 6 * Double_wosize)
    invalid_argument("affine transform");
  gnome_canvas_item_affine_relative(GnomeCanvasItem_val(i), (double *)a);
  return Val_unit;
}

CAMLprim value ml_gnome_canvas_item_affine_absolute(value i, value a)
{
  if(Wosize_val(a) == 0)
    gnome_canvas_item_affine_absolute(GnomeCanvasItem_val(i), NULL);
  else if(Wosize_val(a) == 6 * Double_wosize)
    gnome_canvas_item_affine_absolute(GnomeCanvasItem_val(i), (double *)a);
  else invalid_argument("affine transform");
  return Val_unit;
}
#else
CAMLprim value ml_gnome_canvas_item_affine_relative(value i, value a)
{
  double coords[6];
  if(Wosize_val(a) != 6 * Double_wosize)
    invalid_argument("affine transform");
  memcpy(coords, Bp_val(a), sizeof coords);
  gnome_canvas_item_affine_relative(GnomeCanvasItem_val(i), coords);
  return Val_unit;
}

CAMLprim value ml_gnome_canvas_item_affine_absolute(value i, value a)
{
  double coords[6];
  if(Wosize_val(a) == 0)
    gnome_canvas_item_affine_absolute(GnomeCanvasItem_val(i), NULL);
  else if(Wosize_val(a) == 6 * Double_wosize) {
    memcpy(coords, Bp_val(a), sizeof coords);
    gnome_canvas_item_affine_absolute(GnomeCanvasItem_val(i), coords);
  }
  else invalid_argument("affine transform");
  return Val_unit;
}
#endif /* ARCH_ALIGN_DOUBLE */

CAMLprim value ml_gnome_canvas_item_set(value i)
{
  gnome_canvas_item_set(GnomeCanvasItem_val(i), NULL);
  return Val_unit;
}

ML_3 (gnome_canvas_item_move, GnomeCanvasItem_val, Double_val, Double_val, Unit)
ML_2 (gnome_canvas_item_raise, GnomeCanvasItem_val, Int_val, Unit)
ML_2 (gnome_canvas_item_lower, GnomeCanvasItem_val, Int_val, Unit)
ML_1 (gnome_canvas_item_raise_to_top, GnomeCanvasItem_val, Unit)
ML_1 (gnome_canvas_item_lower_to_bottom, GnomeCanvasItem_val, Unit)
ML_1 (gnome_canvas_item_show, GnomeCanvasItem_val, Unit)
ML_1 (gnome_canvas_item_hide, GnomeCanvasItem_val, Unit)

ML_4 (gnome_canvas_item_grab, GnomeCanvasItem_val, Flags_Event_mask_val, GdkCursor_val, Int32_val, Unit)
ML_2 (gnome_canvas_item_ungrab, GnomeCanvasItem_val, Int32_val, Unit)

CAMLprim value ml_gnome_canvas_item_w2i(value i, value x, value y)
{
  double ox = Double_val(x);
  double oy = Double_val(y);
  gnome_canvas_item_w2i(GnomeCanvasItem_val(i), &ox, &oy);
  return copy_two_doubles(ox, oy);
}

CAMLprim value ml_gnome_canvas_item_i2w(value i, value x, value y)
{
  double ox = Double_val(x);
  double oy = Double_val(y);
  gnome_canvas_item_i2w(GnomeCanvasItem_val(i), &ox, &oy);
  return copy_two_doubles(ox, oy);
}

#ifndef ARCH_ALIGN_DOUBLE
CAMLprim value ml_gnome_canvas_item_i2w_affine(value i)
{
  GnomeCanvasItem *item = GnomeCanvasItem_val(i);
  value v = alloc_small(6 * Double_wosize, Double_array_tag);
  gnome_canvas_item_i2w_affine(item, (double *)v);
  return v;
}

CAMLprim value ml_gnome_canvas_item_i2c_affine(value i)
{
  GnomeCanvasItem *item = GnomeCanvasItem_val(i);
  value v = alloc_small(6 * Double_wosize, Double_array_tag);
  gnome_canvas_item_i2c_affine(item, (double *)v);
  return v;
}
#else
CAMLprim value ml_gnome_canvas_item_i2w_affine(value i)
{
  GnomeCanvasItem *item = GnomeCanvasItem_val(i);
  value v = alloc_small(6 * Double_wosize, Double_array_tag);
  double coords[6];
  gnome_canvas_item_i2w_affine(item, coords);
  memcpy(Bp_val(v), coords, sizeof coords);
  return v;
}

CAMLprim value ml_gnome_canvas_item_i2c_affine(value i)
{
  GnomeCanvasItem *item = GnomeCanvasItem_val(i);
  value v = alloc_small(6 * Double_wosize, Double_array_tag);
  double coords[6];
  gnome_canvas_item_i2c_affine(item, coords);
  memcpy(Bp_val(v), coords, sizeof coords);
  return v;
}
#endif /* ARCH_ALIGN_DOUBLE */

ML_2 (gnome_canvas_item_reparent, GnomeCanvasItem_val, GnomeCanvasGroup_val, Unit)
ML_1 (gnome_canvas_item_grab_focus, GnomeCanvasItem_val, Unit)

CAMLprim value ml_gnome_canvas_item_get_bounds(value i)
{
  double p[4];
  gnome_canvas_item_get_bounds(GnomeCanvasItem_val(i), &p[0], &p[1], &p[2], &p[3]);
  return copy_double_array(p, 4);
}
/* gnome_canvas_item_request_update */


/* GnomeCanvasGroup */

CAMLprim value ml_gnome_canvas_group_get_items(value cg)
{
  return Val_GList(GnomeCanvasGroup_val(cg)->item_list, (value_in)Val_GObject);
}


/* Converion functions for properties */

CAMLprim value ml_gnome_canvas_convert_points(value arr)
{
  int len = Wosize_val(arr) / Double_wosize;
  GnomeCanvasPoints *p;
  if(len % 2)
    invalid_argument("GnomeCanvas.convert_points: odd number of coords");
  p = gnome_canvas_points_new(len / 2);
  memcpy(p->coords, Bp_val(arr), Bosize_val(arr));
  return Val_gboxed_new(gnome_canvas_points_get_type(), p);
}
CAMLprim value ml_gnome_canvas_get_points(value arg)
{
  GnomeCanvasPoints *p = Pointer_val(arg);
  value ret = alloc(p->num_points * 2 * Double_wosize, Double_array_tag);
  memcpy(Bp_val(ret), p->coords, p->num_points * 2 * sizeof(double));
  return ret;
}

#define artvpathdash_free(d) g_free((d)->dash); g_free(d)
Make_Val_final_pointer_ext(ArtVpathDash, _new, Ignore, artvpathdash_free, 1)
CAMLprim value ml_gnome_canvas_convert_dash(value off, value dash)
{
  ArtVpathDash *d;
  int len = Wosize_val(dash) / Double_wosize;
  d = g_malloc(sizeof *d);
  d->offset = Double_val(off);
  d->n_dash = len;
  d->dash = g_malloc(d->n_dash * sizeof (double));
  memcpy(d->dash, Bp_val(dash), Bosize_val(dash));
  return Val_ArtVpathDash_new(d);
}
CAMLprim value ml_gnome_canvas_get_dash(value dash)
{
  CAMLparam1(dash);
  CAMLlocal3(ret,dashes,offset);
  ArtVpathDash *d = Pointer_val(dash);
  dashes = alloc(d->n_dash * Double_wosize, Double_array_tag);
  memcpy(Bp_val(dashes), d->dash, d->n_dash * sizeof (double));
  offset = copy_double(d->offset);
  ret = alloc_small(2, 0);
  Field(ret,0) = offset;
  Field(ret,1) = dashes;
  CAMLreturn (ret);
}



/* gome-canvas-path-def.h */

Make_Val_final_pointer_ext(GnomeCanvasPathDef, _new, Ignore, gnome_canvas_path_def_unref, 1)
#define GnomeCanvasPathDef_val(v) ((GnomeCanvasPathDef *)Pointer_val(v))

CAMLprim value ml_gnome_canvas_path_def_new(value olen, value unit)
{
  gint len = Option_val(olen, Int_val, -1);
  GnomeCanvasPathDef *p;
  if(len < 0)
    p = gnome_canvas_path_def_new();
  else
    p = gnome_canvas_path_def_new_sized(len);
  return Val_GnomeCanvasPathDef_new(p);
}

ML_1 (gnome_canvas_path_def_duplicate, GnomeCanvasPathDef_val, Val_GnomeCanvasPathDef_new)

static gpointer path_def_val(value v)
{
  return GnomeCanvasPathDef_val(v);
}

CAMLprim value ml_gnome_canvas_path_def_concat(value plist)
{
  GSList *l = GSList_val(plist, path_def_val);
  return Val_GnomeCanvasPathDef_new(gnome_canvas_path_def_concat(l));
}

ML_1 (gnome_canvas_path_def_reset, GnomeCanvasPathDef_val, Unit)
ML_3 (gnome_canvas_path_def_moveto, GnomeCanvasPathDef_val, Double_val, Double_val, Unit)
ML_3 (gnome_canvas_path_def_lineto, GnomeCanvasPathDef_val, Double_val, Double_val, Unit)
ML_3 (gnome_canvas_path_def_lineto_moving, GnomeCanvasPathDef_val, Double_val, Double_val, Unit)
ML_7 (gnome_canvas_path_def_curveto, GnomeCanvasPathDef_val, Double_val, Double_val, Double_val, Double_val, Double_val, Double_val, Unit)
ML_bc7 (ml_gnome_canvas_path_def_curveto)
ML_1 (gnome_canvas_path_def_closepath, GnomeCanvasPathDef_val, Unit)
ML_1 (gnome_canvas_path_def_closepath_current, GnomeCanvasPathDef_val, Unit)

ML_1 (gnome_canvas_path_def_length, GnomeCanvasPathDef_val, Val_int)
ML_1 (gnome_canvas_path_def_is_empty, GnomeCanvasPathDef_val, Val_bool)
ML_1 (gnome_canvas_path_def_has_currentpoint, GnomeCanvasPathDef_val, Val_bool)


/* gnome-canvas-rich-text.h */
#define GnomeCanvasRichText_val(val)      check_cast(GNOME_CANVAS_RICH_TEXT,val)

ML_1 (gnome_canvas_rich_text_cut_clipboard, GnomeCanvasRichText_val, Unit)
ML_1 (gnome_canvas_rich_text_copy_clipboard, GnomeCanvasRichText_val, Unit)
ML_1 (gnome_canvas_rich_text_paste_clipboard, GnomeCanvasRichText_val, Unit)
ML_1 (gnome_canvas_rich_text_get_buffer, GnomeCanvasRichText_val, Val_GAnyObject)
