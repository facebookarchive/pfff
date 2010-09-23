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

/* $Id: ml_rsvg.c 1347 2007-06-20 07:40:34Z guesdon $ */
/* Author: Olivier Andrieu */

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <librsvg/rsvg.h>
#include <librsvg/librsvg-features.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_gdkpixbuf.h"
#include "ml_gobject.h"
#include "ml_glib.h"

static
void ml_rsvg_size_callback(gint *w, gint *h, gpointer user_data)
{
  value *cb = user_data;
  value r;
  r = callback2_exn(*cb, Val_int(*w), Val_int(*h));
  if(Is_exception_result(r)) return;
  *w = Int_val(Field(r, 0));
  *h = Int_val(Field(r, 1));
}

ML_0(rsvg_handle_new, Val_pointer)

#ifdef HAVE_SVGZ
#include <librsvg/rsvg-gz.h>
ML_0 (rsvg_handle_new_gz, Val_pointer)
#else
CAMLprim value ml_rsvg_handle_new_gz()
{ failwith ("Doesn't support GZipped SVG files"); return Val_unit; }
#endif /* HAVE_SVGZ */

#define RsvgHandle_val(val) ((RsvgHandle *)Pointer_val(val))

CAMLprim value ml_rsvg_handle_set_size_callback(value vh, value cb)
{
  RsvgHandle *h = RsvgHandle_val(vh);
  value *u_data = ml_global_root_new(cb);
  rsvg_handle_set_size_callback(h, ml_rsvg_size_callback, u_data, ml_global_root_destroy);
  return Val_unit;
}

ML_1(rsvg_handle_free, RsvgHandle_val, Unit)

CAMLprim value ml_rsvg_handle_close(value h)
{
  GError *err = NULL;
  rsvg_handle_close(RsvgHandle_val(h), &err);
  if (err != NULL)
    ml_raise_gerror (err);
  return Val_unit;
}

static inline 
void check_substring(value s, value o, value l)
{
  if(Int_val(o) < 0 || Int_val(l) < 0 || 
     Int_val(o) + Int_val(l) > string_length(s))
    invalid_argument("bad substring");
}

CAMLprim value ml_rsvg_handle_write(value h, value s, value off, value len)
{
  GError *err = NULL;
  check_substring(s, off, len);
  rsvg_handle_write(RsvgHandle_val(h), 
		    (guchar *) String_val(s)+Int_val(off), Int_val(len), &err);
  if (err != NULL)
    ml_raise_gerror (err);
  return Val_unit;
}

ML_1(rsvg_handle_get_pixbuf, RsvgHandle_val, Val_GdkPixbuf_new)

#if (LIBRSVG_MAJOR_VERSION == 2) && (LIBRSVG_MINOR_VERSION >= 2)
ML_2(rsvg_handle_set_dpi, RsvgHandle_val, Double_val, Unit)
ML_1(rsvg_set_default_dpi, Double_val, Unit)
#else
Unsupported(rsvg_handle_set_dpi)
Unsupported(rsvg_set_default_dpi)
#endif

CAMLprim value ml_rsvg_init (value unit)
{
  ml_register_exn_map(RSVG_ERROR, "ml_rsvg_exn");
  return Val_unit;
}
