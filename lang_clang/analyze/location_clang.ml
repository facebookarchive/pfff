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
open Common

open Ast_clang
open Parser_clang
module Ast = Ast_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type location =
  | File of Common.filename * int * int
  | Line of int * int
  | Col of int
  | Other

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let location_of_angle (line, file) xs =
  match xs with
  | [Angle [T (TLowerIdent "invalid"); T (TLowerIdent "sloc")]] ->
      [Other]
  | xs ->
      let xxs = Common.split_gen_when 
        (function (T TComma)::xs -> Some xs | _ -> None) xs in
      xxs +> List.map (function
        | [T (TLowerIdent "line"); T TColon; T (TInt i1);T TColon; T(TInt i2)]->
            Line (s_to_i i1, s_to_i i2)
        | [T (TLowerIdent "col"); T TColon; T (TInt i);] ->
            Col (s_to_i i)
        | [T (TPath f); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            File (f, s_to_i i1, s_to_i i2)
        | [Angle _; T TColon; T (TInt _);T TColon; T (TInt _)] ->
            Other
        | xs -> 
          pr2_gen xs;
          failwith (spf "wrong location format at line %d in %s"  line file)
      )

let readable_of_filename f =
  let xs = Common.split "/" f in
  let xs = 
    match xs with
    | "usr"::"include"::rest -> 
        "EXTERNAL"::"CORE"::rest

    | "Users"::"yoann.padioleau"::"local"::"clang_ast"::"clang-llvm"
      ::"llvm"::"Debug+Asserts"::"bin"::".."
      ::"lib"::"clang"::"3.3"::"include"::rest ->
        "EXTERNAL"::"CLANG"::rest
    | "System"::"Library"::"Frameworks"::rest -> 
        "EXTERNAL"::"MACOS"::rest

    (* todo: use env.dir? *)
    | "home"::"pad"::"local"::"lang-c"::"Chipmunk-Physics"::rest -> 
        rest
    | "home"::"pad"::"pfff"::"tests"::"clang"::"c"::rest ->
        rest
    | _ -> failwith ("unhandled prefix: " ^ f)
  in
  Common.join "/" xs
