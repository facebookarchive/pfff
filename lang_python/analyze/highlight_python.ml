(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Ast_python

module Ast = Ast_python
(*module V = Visitor_python *)

open Highlight_code

module T = Parser_python
module TH = Token_helpers_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let tag_all_tok_with ~tag categ xs = 
  xs +> List.iter (fun tok ->
    let info = TH.info_of_tok tok in
    tag info categ
  )

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
 *)
let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

let lexer_based_tagger = true

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
 *)

let visit_toplevel 
    ~tag_hook
    prefs 
    (*db_opt *)
    (toplevel, toks)
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
      ::T.TCommentNewline (ii2)
      ::T.TComment(ii3)
      ::T.TCommentNewline (ii4)
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
    | T.Tclass ii1::T.TIdent (s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Class (Def2 fake_no_def2));
        aux_toks xs

    | T.Tdef ii1::T.TIdent (s, ii2)::xs ->
        (* todo: actually could be a method if in class scope *)
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Function (Def2 fake_no_def2));
        aux_toks xs

    | x::xs ->
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

    | T.TCommentNewline ii | T.TCommentMisc ii -> ()

    | T.TUnknown ii -> tag ii Error
    | T.EOF ii-> ()

    (* values  *)

    | T.TString (s,ii) ->
        tag ii String
    | T.TChar (s, ii) ->
        tag ii String
    | T.TFloat (s,ii) | T.TInt (s,ii) ->
        tag ii Number

    | T.TComplex (_, ii) ->
        tag ii Number

    (* keywords  *)
    | T.Tdef ii | T.Tlambda ii ->
        tag ii Keyword

    | T.Tif ii | T.Telif ii | T.Telse ii ->
        tag ii KeywordConditional

    | T.Tfor ii | T.Twhile ii
      -> tag ii KeywordLoop

    | T.Ttry ii  | T.Tfinally ii | T.Traise ii| T.Texcept ii
      -> tag ii KeywordExn

    | T.Tclass ii
        -> tag ii KeywordObject

    | T.Timport ii  | T.Tas ii | T.Tfrom ii
        -> tag ii KeywordModule

    | T.Tcontinue ii
    | T.Tbreak ii
    | T.Tyield ii
    | T.Treturn ii
        -> tag ii Keyword

    | T.Tis ii | T.Tin ii  | T.Texec ii
    | T.Tprint ii 
    | T.Tpass ii
    | T.Tassert ii
    | T.Twith ii
    | T.Tdel ii
    | T.Tglobal ii
    | T.Tnot ii
    | T.Tand ii
    | T.Tor ii
        -> tag ii Keyword


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

    | T.TDot (ii)
    | T.TColon (ii)
        ->
        tag ii Punctuation




    | T.TTilde ii
    | T.TStar ii
      -> tag ii Punctuation

    | T.TBackQuote ii
    | T.TAnd ii
        -> tag ii Punctuation

    | T.TAugOp (_, ii) -> tag ii Punctuation

    | T.TAt ii
    | T.TNotEq ii
    | T.TDiff ii
    | T.TLessEq ii
    | T.TMoreEq ii
    | T.TEqEq ii
    | T.TXor ii
    | T.TOr ii
    | T.TPercent ii
    | T.TSlashSlash ii
    | T.TSlash ii
    | T.TStarStar ii
    | T.TEllipsis ii
    | T.TComma ii
    | T.TCAngle ii
    | T.TOAngle ii
        -> tag ii Punctuation


    | T.TIdent (s, ii)
        -> ()

  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  

  ()
