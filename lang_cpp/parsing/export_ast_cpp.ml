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

module J = Json_type 
module M = Meta_ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * It can be useful for people who don't like OCaml to still benefit 
 * from pfff parsing by having at least a JSON-like representation
 * of the Ast, hence this file.
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let for_json = {
  M.full_info = false;
  M.type_info = false;
  M.token_info = false;
}

let string_json_of_program x = 
  x +> Meta_ast_cpp.vof_program ~precision:for_json 
    +> Ocaml.json_of_v +> Json_out.string_of_json
