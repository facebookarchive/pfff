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

(* todo: transform into a map_and_filter_for_typedef
 *)
let filter_for_typedef xs = 
  xs +> Common.exclude (fun tok_ext ->
    match tok_ext.TV.t with
   (* const is a strong signal for having a typedef, so why skip it?
    * because it forces to duplicate rules. We need to infer
    * the type anyway even when there is no const around.
    * todo? maybe could do a special pass first that infer typedef
    * using only const rules, and then remove those const so 
    * have best of both worlds.
    *)

    | Tconst _ | Tvolatile _
      -> true
    | _ -> false
  )
  

(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)

(* comments/cpp-directives removed
 * TODO assume have done TInf_Template and TIdent_ClassnameAsQualifier
 * filtering so can focus on typedef identification.
 *)
let find_view_filtered_tokens xs = 
 let xs = filter_for_typedef xs in

 let rec aux xs =
  match xs with
  | [] -> ()

  | {t=(Tstruct _ | Tunion _ | Tenum _ | Tclass _);_}::{t=TIdent _}::xs ->
      aux xs

  (* xx yy *)
  | ({t=TIdent (s,i1)} as tok1)::{t=TIdent _}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* xx * yy
   * TODO: could be a multiplication too 
   * TODO: more confidence when xx terminates in _t ?
   * TODO: could be xx & y in c++
   *)
  | ({t=TIdent (s,i1)} as tok1)::{t=TMul _}::{t=TIdent _}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* xx ** yy
  *)
  | ({t=TIdent (s,i1)} as tok1)::{t=TMul _}::{t=TMul _}::{t=TIdent _}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs




  (* (xx) yy   and not a if/while before (, and yy can also be a constant *)
  | {t=tok1}::{t=TOPar info1}::({t=TIdent(s, i1)} as tok3)::{t=TCPar info2}
    ::{t = TIdent (_,_) | TInt _ }::xs 
    when not (TH.is_stuff_taking_parenthized tok1) (*  && line are the same ? *)
    ->
      change_tok tok3 (TIdent_Typedef (s, i1));
      (* todo? recurse on bigger ? *)
      aux xs

   (* (xx * )
    * TODO: does not really need the closing paren?
    *)
  | {t=TOPar info1}::({t=TIdent(s, i1)} as tok3)::{t=TMul _}::{t=TCPar _}::xs ->
      change_tok tok3 (TIdent_Typedef (s, i1));
      aux xs

   (* (xx ** ) *)
  | {t=TOPar info1}::({t=TIdent(s, i1)} as tok3)
    ::{t=TMul _}::{t=TMul _}::{t=TCPar _}::xs ->
      change_tok tok3 (TIdent_Typedef (s, i1));
      aux xs

  (* recurse *)
  | x::xs -> aux xs
 in
 aux xs
