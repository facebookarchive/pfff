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

module G = Gui
module K = GdkKeysyms

module M = Model3
module Controller = Controller3

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Final view rendering *)
(*****************************************************************************)

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let mk_gui () =
  let width = 1350 in
  let height = 800 in

  let _w = GWindow.window
    ~title:"CodeGraph"
    ~width ~height
    ~allow_shrink:true ~allow_grow:true
    ()
  in
  raise Todo
