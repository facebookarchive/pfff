/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk/gdk.h>

#include "wrappers.h"
#include "ml_gobject.h"
#include "ml_gdkpixbuf.h"
#include "ml_gdk.h"

wML_1(gdk_cairo_create, GdkDrawable_val, Val_cairo_t)
wML_2(gdk_cairo_set_source_color, cairo_t_val, GdkColor_val, Unit)
wML_2(gdk_cairo_rectangle, cairo_t_val, GdkRectangle_val, Unit)
wML_2(gdk_cairo_region, cairo_t_val, GdkRegion_val, Unit)
wML_4(gdk_cairo_set_source_pixbuf, cairo_t_val, GdkPixbuf_val, Double_val, Double_val, Unit)
