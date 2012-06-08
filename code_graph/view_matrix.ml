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

module CairoH = Cairo_helpers3

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Coordinate system *)
(*****************************************************************************)

(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

(* 'Paint' creates the cairo context and adjust the scaling if needed
 * and then call 'draw' functions.
 *)
let paint w =
  pr2 "View_matrix.paint Todo";
  ()

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

let button_action da w ev =
  raise Todo
