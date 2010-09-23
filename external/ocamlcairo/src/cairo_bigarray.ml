(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

open Bigarray

external bigarray_kind_float : ('a, 'b, c_layout) Array2.t -> bool
  = "ml_bigarray_kind_float"
external bigarray_byte_size  : ('a, 'b, c_layout) Array2.t -> int
  = "ml_bigarray_byte_size"


external image_surface_create : 
  ('a, 'b, c_layout) Array2.t ->
  Cairo.format -> width:int -> height:int -> stride:int ->
  Cairo.image_surface = "ml_cairo_image_surface_create_for_data"


let of_bigarr arr format ~width ~height ~stride =
  if bigarray_kind_float arr
  then invalid_arg "wrong Bigarray kind" ;
  if bigarray_byte_size arr < stride * height
  then invalid_arg "Bigarray too small" ;
  image_surface_create arr format width height stride

let of_bigarr_32 ~alpha (arr : (int32, int32_elt, c_layout) Array2.t) =
  let h = Array2.dim1 arr in
  let w = Array2.dim2 arr in
  of_bigarr arr 
    (if alpha then Cairo.FORMAT_ARGB32 else Cairo.FORMAT_RGB24)
    w h (4 * w)

let of_bigarr_24 (arr : (int, int_elt, c_layout) Array2.t) =
  if Sys.word_size <> 32
  then failwith "your ints have 63 bits" ;
  let h = Array2.dim1 arr in
  let w = Array2.dim2 arr in
  of_bigarr arr
    Cairo.FORMAT_RGB24
    w h (4 * w)

let of_bigarr_8 (arr : (int, int8_unsigned_elt, c_layout) Array2.t) =
  let h = Array2.dim1 arr in
  let w = Array2.dim2 arr in
  of_bigarr arr
    Cairo.FORMAT_A8
    w h w

let output_pixel oc p =
  let r = (p lsr 16) land 0xff in
  output_byte oc r ;
  let g = (p lsr 8) land 0xff in
  output_byte oc g ;
  let b = p land 0xff in
  output_byte oc b 

let write_ppm_int32 oc (arr : (int32, int32_elt, c_layout) Array2.t) =
  let h = Array2.dim1 arr in
  let w = Array2.dim2 arr in
  Printf.fprintf oc "P6 %d %d 255\n" w h ;
  for i=0 to pred h do
    for j=0 to pred w do
      output_pixel oc (Int32.to_int arr.{i, j})
    done
  done ;
  flush oc

let write_ppm_int oc (arr : (int, int_elt, c_layout) Array2.t) =
  let h = Array2.dim1 arr in
  let w = Array2.dim2 arr in
  Printf.fprintf oc "P6 %d %d 255\n" w h ;
  for i=0 to pred h do
    for j=0 to pred w do
      output_pixel oc arr.{i, j}
    done
  done ;
  flush oc
