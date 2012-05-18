(* Yoann Padioleau
 * 
 * Copyright (C) 2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(* floats are the norm in graphics *)
open Common.ArithFloatInfix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* todo: factorize with codemap/cairo_helpers.ml *)


(*****************************************************************************)
(* Text related *)
(*****************************************************************************)

(*****************************************************************************)
(* Distance conversion *)
(*****************************************************************************)

(*****************************************************************************)
(* Surface *)
(*****************************************************************************)

let surface_of_pixmap pm =
  let cr = Cairo_lablgtk.create pm#pixmap in
  Cairo.get_target cr
