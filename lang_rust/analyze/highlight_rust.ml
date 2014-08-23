(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

module Ast = Ast_rust
open Highlight_code
module T = Parser_rust
module TH = Token_helpers_rust

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
 *)
let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

let lexer_based_tagger = true

let is_module_name s = 
  s =~ "[A-Z].*"

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
 *)

let visit_program
    ~tag_hook
    _prefs 
    (*db_opt *)
    (_ast, toks)
  =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *) 

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)

  let rec aux_toks xs = 
    match xs with
    | [] -> ()
    (* a little bit pad specific *)
    |   T.TComment(ii)
      ::T.TCommentNewline (_ii2)
      ::T.TComment(ii3)
      ::T.TCommentNewline (_ii4)
      ::T.TComment(ii5)
      ::xs ->
        let s = Parse_info.str_of_info ii in
        let s5 =  Parse_info.str_of_info ii5 in
        (match () with
        | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection0
        | _ when s =~ ".*------" && s5 =~ ".*------" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection1
        | _ when s =~ ".*####" && s5 =~ ".*####" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection2
        | _ ->
            ()
        );
        aux_toks xs

    (* poor's man identifier tagger *)

    (* defs *)
    | T.Tstruct _ii1::T.TIdent (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Class (Def2 fake_no_def2));
        aux_toks xs

(*
    | (T.Tvoid _ii | T.Tint _ii)
      ::T.TIdent (_s, ii2)
      ::T.TOParen _
      ::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Method (Def2 fake_no_def2));
        aux_toks xs
*)

    |   T.TIdent (s1, ii1)::T.TColonColon _
      ::T.TIdent (_s3, ii3)::T.TIdent (_s4,ii4)::xs 
       ->
        if not (Hashtbl.mem already_tagged ii4) && lexer_based_tagger
        then begin 
          tag ii4 (Field (Def2 fake_no_def2));

          tag ii3 (TypeInt);
          if is_module_name s1 then tag ii1 (Module (Use));
        end;
        aux_toks xs


    (* uses *)

    |   T.TIdent (s1, ii1)::T.TColonColon _
      ::T.TIdent (_s3, ii3)::T.TOParen _::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then begin 
          tag ii3 (Method (Use2 fake_no_use2));
          (*
          if not (Hashtbl.mem already_tagged ii1)
          then tag ii1 (Local Use);
          *)
          if is_module_name s1 then tag ii1 (Module (Use))
        end;
        aux_toks xs

    |   T.TIdent (s1, ii1)::T.TArrow _
      ::T.TIdent (_s3, ii3)::T.TEq _::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then begin 
          tag ii3 (Field (Use2 fake_no_use2));
          if is_module_name s1 then tag ii1 (Module (Use))
        end;
        aux_toks xs


    |  T.TIdent (s1, ii1)::T.TColonColon _
     ::T.TIdent (s3, ii3)::T.TArrow ii4::xs ->
        if not (Hashtbl.mem already_tagged ii1) && lexer_based_tagger
        then begin 
          if is_module_name s1 then tag ii1 (Module Use)
        end;
        aux_toks (T.TIdent (s3, ii3)::T.TArrow ii4::xs)
        

    | _x::xs ->
        aux_toks xs
  in
  let toks' = toks +> Common.exclude (function
    | T.TCommentSpace _ -> true
    | _ -> false
  )
  in
  aux_toks toks';

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)

  toks +> List.iter (fun tok -> 
    match tok with

    (* comments *)

    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Comment
    | T.TCommentSpace ii ->
        if not (Hashtbl.mem already_tagged ii)
        then ()
        else ()
    | T.TCommentNewline _ii | T.TCommentMisc _ii -> ()
    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii -> ()

    (* values  *)

    | T.TString (_s,ii) ->
        tag ii String
    | T.TChar (_s, ii) ->
        tag ii String
    | T.TFloat (_s,ii) | T.TInt (_s,ii) ->
        tag ii Number

    (* keywords  *)

    | T.Tstruct ii
    | T.Ttrait ii
    | T.Timpl ii
    | T.Tself ii
    | T.Tsuper ii
        -> tag ii KeywordObject

    | T.Tpub ii
    | T.Tpriv ii
        -> tag ii Keyword

    | T.Treturn ii
    | T.Tbreak ii
    | T.Tcontinue ii
        -> tag ii Keyword

    | T.Tmatch ii
        -> tag ii KeywordConditional

    | T.Tcrate ii
    | T.Tuse ii
    | T.Tmod ii
        -> tag ii KeywordModule

    | T.Tstatic ii
    | T.Textern ii

    | T.Tif ii  | T.Telse ii -> tag ii KeywordConditional
    | T.Twhile ii | T.Tfor ii | T.Tloop ii
          -> tag ii KeywordLoop

    | T.Ttrue ii | T.Tfalse ii
      ->  tag ii Boolean

    | T.Tenum ii
    | T.Ttype ii
        -> tag ii Keyword

    | T.Tlet ii
    | T.Tfn ii
    | T.Tas ii
    | T.Tin ii
        -> tag ii Keyword

    | T.Tproc ii
        -> tag ii Keyword

    | T.Tmut ii
        -> tag ii UseOfRef

    | T.Tref ii
    | T.Tbox ii
        -> tag ii Keyword

    | T.Tunsafe ii
        -> tag ii BadSmell


    | T.TCppLine ii
        -> tag ii CppOther

    (* symbols *)
    | T.TEq ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Punctuation

    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii
          -> tag ii Punctuation

    | T.TPlus ii | T.TMinus ii
    | T.TLess ii | T.TMore ii
        -> tag ii Operator

    | T.TArrow (ii)
    | T.TColonColon (ii)
    | T.TPound (ii)
        ->
        tag ii Punctuation

    | T.TTilde ii
    | T.TStar ii
      -> tag ii Operator

    | T.TAnd ii
        -> tag ii Operator

    | T.TAssignOp (_, ii) -> tag ii Punctuation

    | T.TNotEq ii
    | T.TLessEq ii
    | T.TMoreEq ii
    | T.TEqEq ii

    | T.TXor ii
    | T.TOr ii
    | T.TPercent ii
        -> tag ii Operator

    | T.TComma ii
    | T.TCAngle ii
    | T.TOAngle ii
        -> tag ii Punctuation

    | T.TDiv ii
    | T.TOrOr ii 
    | T.TAndAnd ii 
        -> tag ii Operator

    | T.TSemiColon ii
        -> tag ii Punctuation

    | T.TIdent (_s, _ii) -> 
        ()
  );
  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  

  ()
