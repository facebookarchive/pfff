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
  let cnt = ref 0 in

  (* transformation to not have the parse info or type info in the output *)
  let v' = Ocaml.map_v ~f:(fun ~k x ->
    match x with
    | Ocaml.VDict (xs) ->
        incr cnt;
        (match () with
        | _ when xs +> List.exists (function ("token", _) ->true | _ -> false)->
            Ocaml.VVar ("i", Int64.of_int !cnt)
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

let ml_pattern_string_of_program ast = 
  Meta_ast_php.vof_program ast +> string_of_v

let ml_pattern_string_of_expr e = 
  Meta_ast_php.vof_expr e +> string_of_v

let ml_pattern_string_of_any any =
  Meta_ast_php.vof_any any +> string_of_v

(*e: export_ast_php.ml *)
