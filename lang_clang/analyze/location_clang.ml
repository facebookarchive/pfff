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
(*
 * Assumes 'clang-check --ast-dump' dumps realpath paths. Modify
 * ASTDumper.cpp for that, see clang.patch in this directory.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type location =
  | File of Common.filename * int * int
  | Line of int * int
  | Col of int
  | Other

let unknown_loc_angle =
  Angle [Angle [T (TLowerIdent "invalid"); T (TLowerIdent "sloc")]]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let location_of_angle (line, file) xs =
  match xs with
  | [Angle [T (TLowerIdent "invalid"); T (TLowerIdent "sloc")]] ->
      [Other]
  | xs ->
      let xxs = Common2.split_gen_when 
        (function (T TComma)::xs -> Some xs | _ -> None) xs in
      xxs +> List.map (function
        | [T (TLowerIdent "line"); T TColon; T (TInt i1);T TColon; T(TInt i2)]->
            Line (s_to_i i1, s_to_i i2)
        | [T (TLowerIdent "col"); T TColon; T (TInt i);] ->
            Col (s_to_i i)
        (* less: #line directive, ignore? use ExpansionLoc in ASTDumper? *)
        | [T (TPath f); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            File (f, s_to_i i1, s_to_i i2)
        | [Angle _; T TColon; T (TInt _);T TColon; T (TInt _)] ->
            Other
        | xs -> 
          pr2_gen xs;
          failwith (spf "wrong location format at line %d in %s"  line file)
      )

let readable_of_filename ~root f =
  let xs = Common.split "/" f in
  let xs = 
    match xs with
    | "usr"::"include"::rest -> 
        "EXTERNAL"::"CORE"::rest

    (* macos specific *)
    | "System"::"Library"::"Frameworks"::rest -> 
        "EXTERNAL"::"MACOS"::rest
    | "opt"::"local"::rest ->
        "EXTERNAL"::"OPT"::rest

    (* llvm install specific on macos *)
    | "Users"::"yoann.padioleau"::"local"::"clang_ast"::"clang-llvm"
      ::"llvm"::"Debug+Asserts"::"lib"::"clang"::"3.3"::"include"::rest ->
        "EXTERNAL"::"CLANG"::rest

    | _ ->
        Common.split "/" (Common.filename_without_leading_path root f)
  in
  Common.join "/" xs

let location_of_paren_opt ~root clang_file (enum, l, xs) =
  let location =
    match enum, xs with
    | (Misc__Null__ | Misc__Capture__ | Misc__Cleanup__Block
      |Field (* TODO: when under IndirectDecl *)
      ), _ -> [Other]
    | _, Angle xs::_rest ->
        location_of_angle (l, clang_file) xs
    | _ -> 
        failwith (spf "%s:%d: no location" clang_file l)

  in
  location +> Common.find_some_opt (function 
  | File (f, _,_) ->
      let readable = readable_of_filename ~root f in
      (* ugly: stdbool.h contains some macros that then confused
       * the unincluder
       *)
(*
      if readable =$= "EXTERNAL/CLANG/stdbool.h"
      then None
      else 
*)
        Some readable
  | _ -> None
  )


(* for anon struct, union, enums, to have a stable unique name based
 * on the location of the struct, which should be unique enough ...
 * todo: put also the filename or a md5sum of the filename so
 * sure unique?
 *)
let str_of_angle_loc line paren current_clang_file =
  let loc =
    match paren with
    | Angle xs ->
        location_of_angle (line, current_clang_file) xs
    | _ ->
        failwith (spf "%s:%d: no location" current_clang_file line)
  in
  "__" ^ current_clang_file ^
  match loc with
  | Line (i1, i2)::_ -> spf "__line_%d_%d" i1 i2
  | File (_, i1, i2)::_ -> spf "__line_%d_%d" i1 i2
  | Col i::_ -> spf "__col_%d" i
  | _ -> 
      failwith (spf "%s:%d: no Line location" current_clang_file line)
