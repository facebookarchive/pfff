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
      let xxs = Common.split_gen_when 
        (function (T TComma)::xs -> Some xs | _ -> None) xs in
      xxs +> List.map (function
        | [T (TLowerIdent "line"); T TColon; T (TInt i1);T TColon; T(TInt i2)]->
            Line (s_to_i i1, s_to_i i2)
        | [T (TLowerIdent "col"); T TColon; T (TInt i);] ->
            Col (s_to_i i)
        | [T (TPath f); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            File (f, s_to_i i1, s_to_i i2)

       (* once ASTDumper.cpp has been modified to use realpath, we can skip
        * this ugly code
        *)
(*             
        | [T TDot; T TDot; T (TPath f); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            let f = Filename.concat (Common.dirname pwd) f in
            File (f, s_to_i i1, s_to_i i2)
        | [T TDot; T (TPath f); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            let f = Filename.concat pwd f in
            File (f, s_to_i i1, s_to_i i2)
        (* #line directive, ignore it? *)
        | [T (TLowerIdent "lex"); T TDot; T (TLowerIdent "yy");T TDot;
           T (TLowerIdent "c"); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            let f = Filename.concat pwd "lex.yy.c" in
            File (f, s_to_i i1, s_to_i i2)
        | [T (TLowerIdent "parser_yacc"); T TDot;
           T (TLowerIdent "c"); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            let f = Filename.concat pwd "parser_yacc.c" in
            File (f, s_to_i i1, s_to_i i2)
*)
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
(*
    (* todo: use env.dir? *)
    | "home"::"pad"::"pfff"::"tests"::"clang"::"c"::rest ->
        rest
    | "home"::"pad"::"local"::"lang-c"::"Chipmunk-Physics"::rest -> 
        rest
    | "home"::"pad"::"local"::"lang-c"::"Bear"::rest -> 
        rest
    | "Users"::"yoann.padioleau"::"local"::"lang-objc"::"objc"::"hello"::rest
        -> rest
    | "Users"::"yoann.padioleau"::"software-src"::"tool-other"::"sparse"::rest
        -> rest

    | "Users"::"yoann.padioleau"::"software-src"::"XIX"::"compiler-byacc"::rest
        -> rest
    | "Users"::"yoann.padioleau"::"software-src"::"XIX"::"compiler-tiny-cc"::rest
        -> rest
    | "Users"::"yoann.padioleau"::"software-src"::"tool-other"::"ctags-cscope"::"ctags-5.8"::rest ->
        rest
    | "Users"::"yoann.padioleau"::"software-src"::"EDU"::"editor-nano"::rest ->
        rest
    | "home"::"pad"::"software-src"::"BIG"::"machine-qemu"::rest -> 
        rest
    | "Users"::"yoann.padioleau"::"software-src"::"BIG"::"machine-qemu"::rest ->
        rest

    | ".."::"CPU"::rest ->
        "CPU"::rest
    | "Users"::"yoann.padioleau"::"local"::"lang-c"::"spimsimulator"::rest ->
        rest
*)
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
      Some readable
  | _ -> None
  )

