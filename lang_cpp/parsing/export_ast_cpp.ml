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

let no_info = 
  for_json

let string_json_of_program x = 
  x |> Meta_ast_cpp.vof_program for_json 
    |> Ocaml.json_of_v |> Json_out.string_of_json

(*****************************************************************************)
(* ML Patterns *)
(*****************************************************************************)

let string_of_v precision v =
  let cnt = ref 0 in

  (* transformation to not have the parse info or type info in the output *)
  let v' = Ocaml.map_v ~f:(fun ~k x ->
    match x with
    | Ocaml.VDict (xs) ->
        incr cnt;
        (match () with
        | _ when xs +> List.exists (function ("token", _) -> true | _ -> false)->
            if not precision.M.token_info
            then Ocaml.VVar ("i", Int64.of_int !cnt)
            else x
        | _ when xs +> List.exists (function ("t", _) -> true | _ -> false)->
            Ocaml.VVar ("t", Int64.of_int !cnt)
        | _ when xs +> List.exists (function ("tvar", _) -> true | _ -> false)->
            Ocaml.VVar ("tlval", Int64.of_int !cnt)
        | _ -> 
            (* recurse, x can be a record containing itself some records *)
            k x
        )
    | _ -> k x
  ) v
  in

  let s = Ocaml.string_of_v v' in
  s


let ml_pattern_string_of_program ?(precision=no_info) ast = 
  Meta_ast_cpp.vof_program precision ast +> string_of_v precision
