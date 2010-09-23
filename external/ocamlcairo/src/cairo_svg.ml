(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

type surface = [`Any|`SVG] Cairo.surface

external surface_create_for_stream_unsafe : 
  (string -> int -> unit) ->
  width_in_points:float -> 
  height_in_points:float -> surface = "ml_cairo_svg_surface_create_for_stream_unsafe"

let unsafe_output_string oc s n =
  for i = 0 to n - 1 do
    output_char oc (String.unsafe_get s i)
  done

let surface_create_for_channel oc ~width_in_points ~height_in_points =
  surface_create_for_stream_unsafe
    (unsafe_output_string oc) ~width_in_points ~height_in_points

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
