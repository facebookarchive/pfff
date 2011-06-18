(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011 Facebook
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

module TV = Token_views_cpp

open Parser_cpp
open Token_views_cpp

open Parsing_hacks_lib

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: use a view that is more convenient for typedefs detection.
 *  get rid of template arguments, get rid of qualifier, get
 *  rid of differences between & and *, differences between
 *  TIdent and TOperator, get rid of const, inline, merge multiple
 *  ** or *& or whatever
 * 
 * At the same time certain tokens like const/inline are stronger
 * signals towards a typedef ident.
 *)
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_top_or_struct = function
  | TV.InTopLevel
  | TV.InClassStruct _ 
  | TV.InStructAnon
      -> true
  | _ -> false

(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)

(* comments/cpp-directives removed
 * TODO assume have done TInf -> TInf_Template
 *)
let find_view_filtered_tokens xs = 
 let rec aux xs =
  match xs with
  | [] -> ()

  | {t=(Tstruct _ | Tunion _ | Tenum _ | Tclass _);_}::{t=TIdent _}::xs ->
      aux xs

  (* xx yy *)
  | ({t=TIdent (s,i1)} as tok1)::{t=TIdent _}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* recurse *)
  | x::xs -> aux xs
 in
 aux xs
