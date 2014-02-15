(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

open Ast_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type visitor = 
  (Ast_clang.sexp -> unit) -> Ast_clang.sexp -> unit

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let visit hook x =

  let rec sexp x = 
    let k x =
      match x with
      | Paren (_, _, xs) | Angle xs | Anchor xs | Bracket xs ->
          List.iter sexp xs
      | Brace (_toks, _) -> ()
      | T _ -> ()
    in
    hook k x
  in
  sexp x

