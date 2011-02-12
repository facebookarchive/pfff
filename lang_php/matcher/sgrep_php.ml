(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Ast_php
open Parse_info

module Ast = Ast_php
module V = Visitor_php

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Sgrep
*)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* but right now only Expr and Stmt are supported *)
type pattern = Ast_php.any

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let parse str =
  Common.save_excursion Flag_parsing_php.sgrep_mode true (fun () ->
    Parse_php.any_of_string str
  )
