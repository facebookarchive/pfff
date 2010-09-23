(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

external image_surface_create_from_stream_unsafe : 
  (string -> int -> unit) -> Cairo.image_surface = "ml_cairo_image_surface_create_from_png_stream_unsafe"

let image_surface_create_from_channel ic =
  image_surface_create_from_stream_unsafe
    (fun s n ->
      for i = 0 to n - 1 do
	String.unsafe_set s i (input_char ic)
      done)

let image_surface_create_from_file fname =
  let ic = open_in fname in
  try
    let surf = image_surface_create_from_channel ic in
    close_in ic ;
    surf
  with exn ->
    close_in_noerr ic ;
    raise exn

external image_surface_create_from_stream : 
  (string -> unit) -> Cairo.image_surface = "ml_cairo_image_surface_create_from_png_stream"



external surface_write_to_stream_unsafe : 
  [> `Any] Cairo.surface -> (string -> int -> unit) -> unit = "ml_cairo_surface_write_to_png_stream_unsafe"

let unsafe_output_string oc s n =
  for i = 0 to n - 1 do
    output_char oc (String.unsafe_get s i)
  done

let surface_write_to_channel surf oc =
  surface_write_to_stream_unsafe 
    surf
    (unsafe_output_string oc)

let surface_write_to_file surf fname =
  let oc = open_out_bin fname in
  try
    surface_write_to_channel surf oc ;
    close_out oc
  with exn ->
    close_out_noerr oc ;
    raise exn

external surface_write_to_stream : 
  [> `Any] Cairo.surface -> (string -> unit) -> unit = "ml_cairo_surface_write_to_png_stream"
