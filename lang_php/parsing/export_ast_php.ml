(*s: export_ast_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2009-2011 Facebook
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
open Common

(*s: json_ast_php.ml *)
(* module J = Json_type *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * It can be useful for people who don't like OCaml to still benefit
 * from pfff parsing by having at least a JSON representation
 * of the Ast, hence this file. Other parts of pfff generates JSON
 * data (see flib_navigator/, fb_phpunit/, h_visualization/).
 *
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*
let json_string_of_expr x =
  x +> Meta_ast_php.vof_expr +> Ocaml.json_of_v +> Json_out.string_of_json
let json_string_of_toplevel x =
  x +> Meta_ast_php.vof_toplevel +> Ocaml.json_of_v +> Json_out.string_of_json
let json_string_of_program x =
  Common.profile_code "json_of_program" (fun () ->
    x +> Meta_ast_php.vof_program +> Ocaml.json_of_v +> Json_out.string_of_json
  )

let json_string_of_program_fast x =
  Common.profile_code "json_of_program_fast" (fun () ->
    let json = x +> Meta_ast_php.vof_program +> Ocaml.json_of_v
    in
    Common.profile_code "string_of_json" (fun () ->
      Json_io.string_of_json ~compact:true ~recursive:false json
    )
  )
*)
(*e: json_ast_php.ml *)
(*s: sexp_ast_php.ml *)
(*e: sexp_ast_php.ml *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  let v = Ocaml.VList [
    Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000;
    Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000;
    Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000;
    Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000; Ocaml.VInt 10000;
    Ocaml.VSum ("Foo", [Ocaml.VInt 10000; Ocaml.VInt 10000;]);
    ]
  in

  let s = Ocaml.string_of_v v in
  pr2 s;
*)

let string_of_v v =
  let cnt_i = ref 0 in
  let cnt_other = ref 0 in

  (* transformation to not have the parse info or type info in the output *)
  let v' = Ocaml.map_v ~f:(fun ~k x ->
    match x with
    | Ocaml.VDict (xs) ->
        (match () with
        | _ when xs +> List.exists (function ("token", _) ->true | _ -> false)->
            incr cnt_i;
            Ocaml.VVar ("i", Int64.of_int !cnt_i)
        | _ when xs +> List.exists (function ("t", _) -> true | _ -> false)->
            incr cnt_other;
            Ocaml.VVar ("t", Int64.of_int !cnt_other)
        | _ when xs +> List.exists (function ("tvar", _) -> true | _ -> false)->
            incr cnt_other;
            Ocaml.VVar ("tlval", Int64.of_int !cnt_other)
        | _ ->
            (* recurse, x can be a record containing itself some records *)
            k x
        )
    | _ -> k x
  ) v
  in
  let s = Ocaml.string_of_v v' in
  s

let ml_pattern_string_of_program ast =
  Meta_ast_php.vof_program ast +> string_of_v

let ml_pattern_string_of_expr e =
  Meta_ast_php.vof_expr e +> string_of_v

let ml_pattern_string_of_typehint th =
  Meta_ast_php.vof_hint_type th +> string_of_v

let ml_pattern_string_of_any any =
  Meta_ast_php.vof_any any +> string_of_v

(*e: export_ast_php.ml *)
