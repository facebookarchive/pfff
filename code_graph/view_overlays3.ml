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

(* This module mainly modifies the w.overlay cairo surface. It also
 * triggers the refresh_da which triggers itself the expose event
 * which triggers the View.assemble_layers composition of w.pm with
 * w.overlay
 *)

(*****************************************************************************)
(* The overlays *)
(*****************************************************************************)

(*****************************************************************************)
(* Assembling overlays *)
(*****************************************************************************)

let motion_notify da w ev =
  pr2 "View_overlay: motion_notify Todo";
  true

