(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

(** SVG backend *)

type surface = [`Any|`SVG] Cairo.surface

val surface_create_for_channel : 
  out_channel -> 
  width_in_points:float -> 
  height_in_points:float -> surface

external surface_create_for_stream :
  (string -> unit) ->
  width_in_points:float -> 
  height_in_points:float -> surface = "ml_cairo_svg_surface_create_for_stream"

type version =
  | VERSION_1_1
  | VERSION_1_2

external restrict_to_version :
  [> `SVG] Cairo.surface -> version -> unit = "ml_cairo_svg_surface_restrict_to_version"

external string_of_version : version -> string = "ml_cairo_svg_version_to_string"
