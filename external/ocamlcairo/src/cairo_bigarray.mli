(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

(** image backend, via Bigarray *)

open Bigarray

val of_bigarr :
  ('a, 'b, c_layout) Array2.t -> Cairo.format -> 
  width:int -> height:int -> stride:int -> Cairo.image_surface

val of_bigarr_32 : alpha:bool -> (int32, int32_elt, c_layout) Array2.t -> Cairo.image_surface
val of_bigarr_24 : (int, int_elt, c_layout) Array2.t -> Cairo.image_surface
val of_bigarr_8  : (int, int8_unsigned_elt, c_layout) Array2.t -> Cairo.image_surface

val write_ppm_int32 : out_channel -> (int32, int32_elt, c_layout) Array2.t -> unit
val write_ppm_int   : out_channel -> (int,   int_elt,   c_layout) Array2.t -> unit
