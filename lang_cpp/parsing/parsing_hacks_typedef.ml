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
module TH = Token_helpers_cpp
module Ast = Ast_cpp

open Parser_cpp
open Token_views_cpp

open Parsing_hacks_lib

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This file gathers parsing heuristics related to the typedefs.
 * C is not context-free sensitive; it requires to know when
 * an ident corresponds to a typedef or ident. This normally means that
 * we must call cpp on the file and have the lexer and parser cooperates
 * to remember what is what. In lang_cpp/ we want to parse as-is,
 * which means we need to infer back whether an identifier is
 * a typedef or not.
 * 
 * In this module we use a view that is more convenient for 
 * typedefs detection. We got rid of:
 *  - template arguments, 
 *  - qualifiers, 
 *  - differences between & and *, 
 *  - differences between TIdent and TOperator, 
 *  - const, volatile, restrict keywords
 *  - TODO merge multiple ** or *& or whatever
 * 
 * See find_template_commentize() and find_qualifier_commentize() in
 * parsing_hacks_cpp.ml
 * 
 * todo? at the same time certain tokens like const are strong
 * signals towards a typedef ident, so maybe could do a first
 * pass first which use those tokens?
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
let is_top_or_struct = function
  | TV.InTopLevel
  | TV.InClassStruct _ 
  | TV.InStructAnon
      -> true
  | _ -> false
*)

let look_like_multiplication tok =
  match tok with
  | TEq _ | TAssign _
  | TWhy _
  | Treturn _
  | TDot _ | TPtrOp _ | TPtrOpStar _ | TDotStar _
  | TOCro _
   -> true
  | tok when TH.is_binary_operator_except_star tok -> true
  | _ -> false

let look_like_declaration tok =
  match tok with
  | TOBrace _ | TCBrace _
  | TPtVirg _
      -> true
  | _ -> false

(*****************************************************************************)
(* Better View *)
(*****************************************************************************)

(* 
 * TODO: right now this is less useful because we actually
 *  comment template args in a previous pass, but at some point this
 *  will be useful.
*)
let rec filter_for_typedef multi_groups = 

  (* a sentinel, which helps a few typedef heuristics which look
   * for a token before which would not work for the first toplevel
   * declaration.
   *)
  let multi_groups = 
    Tok(mk_token_fake (TPtVirg (Ast.fakeInfo())))::multi_groups in

  let _template_args = ref [] in

  (* remove template *)
  let rec aux xs =
    xs +> Common.map_filter (function
    | TV.Braces (t1, xs, t2) ->
        Some (TV.Braces (t1, aux xs, t2))
    | TV.Parens  (t1, xs, t2) ->
        Some (TV.Parens (t1, aux xs, t2))
    | TV.Angle (t1, xs, t2) ->
        (* todo: analayze xs!! add in _template_args 
         * todo: add the t1,t2 around xs to have
         *  some sentinel for the typedef heuristics patterns
         *  who often look for the token just before the typedef.
         *)
        None

    (* remove other noise for the typedef inference *)
    | TV.Tok t1 -> 
        match t1.TV.t with
        (* const is a strong signal for having a typedef, so why skip it?
         * because it forces to duplicate rules. We need to infer
         * the type anyway even when there is no const around.
         * todo? maybe could do a special pass first that infer typedef
         * using only const rules, and then remove those const so 
         * have best of both worlds.
         *)
        | Tconst _ | Tvolatile _
        | Trestrict _
          -> None

        | Tregister _ | Tstatic _ | Tauto _ | Textern _
        | Ttypedef _
        | Tunion _
          -> None

        | Tvirtual _ | Tfriend _ | Tinline _ | Tmutable _
          -> None

        (* let's transform all '&' into '*' *)
        | TAnd ii -> Some (TV.Tok (mk_token_extended (TMul ii)))

        (* and operator into TIdent 
         * TODO: skip the token just after the operator keyword?
         * could help some heuristics too
        *)
        | Toperator ii -> 
            Some (TV.Tok (mk_token_extended (TIdent ("operator", ii))))

        | _ -> Some (TV.Tok t1)
    )
  in
  let xs = aux multi_groups in
  (* todo: look also for _template_args *)
  [TV.tokens_of_multi_grouped xs]

(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)

(* assumes a view without:
 *  - comments and cpp-directives
 *  - template stuff and qualifiers
 *   (but not TIdent_ClassnameAsQualifier)
 *  - const/volatile/restrict
 *  - & => *
 *  - etc, see Prelude
 * 
 * With such a view we can write less patterns.
 * 
 * Note that qualifiers are slightly less important to filter because
 * most of the heuristics below look for tokens after the ident
 * and qualifiers are usually before.
 *)
let find_typedefs xxs = 

 let rec aux xs =
  match xs with
  | [] -> ()

  (* those identifiers (called tags) must not be transformed in typedefs *)
  | {t=(Tstruct _ | Tunion _ | Tenum _ | Tclass _);_}::{t=TIdent _}::xs ->
      aux xs

  (* xx yy *)
  | ({t=TIdent (s,i1)} as tok1)::{t=TIdent _}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* xx * yy  with a token before like ., return, etc that probably mean
   * it's a mulitplication
   *)
  | {t=tok_before}::{t=TIdent (s,i1)}::{t=TMul _}::{t=TIdent _}::xs
    when look_like_multiplication tok_before ->
      aux xs

  (* { xx * yy,  probably declaration
   * TODO if first declaration in file?
   *)
  | {t=tok_before}::({t=TIdent (s,i1)} as tok1)::{t=TMul _}::{t=TIdent _}::xs
    when look_like_declaration tok_before ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* public: x * y *)
  | {t=privacy}::{t=TCol _}::
      ({t=TIdent (s,i1)} as tok1)::{t=TMul _}::{t=TIdent _}::xs 
    when TH.is_privacy_keyword privacy ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* xx * yy
   *
   * could be a multiplication too, so cf rule before and guard
   * with InParameter.
   *)
  | ({t=TIdent (s,i1);where=InParameter::_} as tok1)::{t=TMul _}
    ::{t=TIdent _}::xs 
    ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs


  (* xx ** yy
   * TODO: could be a multiplication too, but with less probability
  *)
  | ({t=TIdent (s,i1)} as tok1)::{t=TMul _}::{t=TMul _}::{t=TIdent _}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs



  (* (xx) yy   and not a if/while before (, and yy can also be a constant *)
  | {t=tok1}::{t=TOPar info1}::({t=TIdent(s, i1)} as tok3)::{t=TCPar info2}
    ::{t = TIdent (_,_) | TInt _ | TString _ | TFloat _ }::xs 
    when not (TH.is_stuff_taking_parenthized tok1) (*  && line are the same ? *)
    ->
      change_tok tok3 (TIdent_Typedef (s, i1));
      (* todo? recurse on bigger ? *)
      aux xs

   (* (xx * )
    * TODO: does not really need the closing paren?
    * TODO: check that not InParameter or InArgument?
    *)
  | {t=TOPar info1}::({t=TIdent(s, i1)} as tok3)::{t=TMul _}::{t=TCPar _}::xs ->
      change_tok tok3 (TIdent_Typedef (s, i1));
      aux xs

   (* (xx ** ) *)
  | {t=TOPar info1}::({t=TIdent(s, i1)} as tok3)
    ::{t=TMul _}::{t=TMul _}::{t=TCPar _}::xs ->
      change_tok tok3 (TIdent_Typedef (s, i1));
      aux xs

  (* xx* [,)] *)
  | ({t=TIdent(s, i1)} as tok1)::{t=TMul _}::{t=(TComma _| TCPar _)}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* xx** [,)] *)
  | ({t=TIdent(s, i1)} as tok1)::{t=TMul _}::{t=TMul _}
    ::{t=(TComma _| TCPar _)}::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* [(,] xx [),] where InParameter *)
  | {t=(TOPar _ | TComma _)}::({t=TIdent (s, i1); where=InParameter::_} as tok1)
    ::({t=(TCPar _ | TComma _)} as tok2)::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux (tok2::xs)


  (* new Xxx (c++ specific, grammar expect a typedef here) *)
  | {t=Tnew _}::({t=TIdent (s, i1)} as tok1)::xs ->
      change_tok tok1 (TIdent_Typedef (s, i1));
      aux xs

  (* recurse *)
  | x::xs -> aux xs
 in
 xxs +> List.iter aux
