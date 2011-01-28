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

module Ast = Ast_php

module V = Visitor_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* There are many places where we need to get access to the list of
 * entities defined in a file or used in a file.
 * 
 * todo: factorize code in
 *  - check_module.ml
 *  - lib_parsing_php manu get_xxx_any
 *  - database_php_build.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type def = unit
type use = unit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let defs_of_any any =
  raise Todo

let uses_of_any any =
  raise Todo
