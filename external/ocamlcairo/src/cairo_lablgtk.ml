(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

external create           : [> `drawable] Gobject.obj -> Cairo.t = "ml_gdk_cairo_create"
external set_source_color : Cairo.t -> Gdk.color -> unit = "ml_gdk_cairo_set_source_color"
external rectangle        : Cairo.t -> Gdk.Rectangle.t -> unit = "ml_gdk_cairo_rectangle"
external region           : Cairo.t -> Gdk.region -> unit = "ml_gdk_cairo_region"

external set_source_pixbuf : Cairo.t -> GdkPixbuf.pixbuf -> float -> float -> unit = "ml_gdk_cairo_set_source_pixbuf"
