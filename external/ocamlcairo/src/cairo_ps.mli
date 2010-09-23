(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

(** PostScript backend *)

type surface = [`Any|`PS] Cairo.surface

val surface_create_for_channel : 
  out_channel -> 
  width_in_points:float -> 
  height_in_points:float -> surface

external surface_create_for_stream :
  (string -> unit) ->
  width_in_points:float -> 
  height_in_points:float -> surface = "ml_cairo_ps_surface_create_for_stream"

external set_size :
  [> `PS] Cairo.surface -> width_in_points:float -> height_in_points:float -> unit = "ml_cairo_ps_surface_set_size"
