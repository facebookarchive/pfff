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

open Common 

open Pil

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Given a filename, inferring the types of the variables in this file
 * requires to understand function calls, field access of entities
 * defined in other files. Moreover it requires to know the
 * types of builtins. Enter 'env' which given the name of
 * an external entity will return its type.
 * 
 *)

type env = unit

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let infer_types env pil = 
  pr2 "infer_types:Todo"

