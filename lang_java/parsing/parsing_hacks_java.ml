(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Parser_java

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module transforms certain tokens like '<', normally a LT
 * into a LT2, which helps solving conflicts in the original
 * Java grammar.
 * 
 * This is similar to what we do for C/C++. 
 * See pfff/lang_cpp/parsing/parsing_hacks.ml for more information.
 *)



(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let rec fix_tokens xs =
  match xs with
  | [] -> []

  (* less: allow also a small space, but usually we should fix
   * this code.
   *)
  | IDENTIFIER (s, ii1)::LT ii2::xs when s =~ "^[A-Z]"->
      IDENTIFIER (s, ii1)::LT2 ii2::fix_tokens xs
      
  | x::xs -> x::fix_tokens xs
