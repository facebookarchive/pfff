(*s: tainted_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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
(*e: Facebook copyright *)
(* Contributions by Alok Menghrajani *)

open Common 

open Ast_php

module Ast = Ast_php

module D = Dataflow_php
module F = Controlflow_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type tainted = bool

type env = tainted D.env

type inout = tainted D.inout

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (tainted_analysis: F.flow -> tainted D.mapping) = fun flow ->
  raise Todo

(*****************************************************************************)
(* Giving warnings about dangerous code  *)
(*****************************************************************************)

let (check_bad_echo: F.flow -> tainted D.mapping -> unit) = 
 fun flow mapping ->
   raise Todo


(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let (display_tainted_flow: Controlflow_php.flow -> tainted Dataflow_php.mapping -> unit) =
 fun flow mapping ->
   raise Todo

(*e: tainted_php.ml *)
