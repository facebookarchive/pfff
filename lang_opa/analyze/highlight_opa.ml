(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

open Ast_opa
open Highlight_code

module Ast = Ast_opa
module T = Parser_opa
module TH = Token_helpers_opa
(*module V = Visitor_opa *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in codemap.
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

    (* defs *)
(*
    | T.Tclass ii1::T.TIdent (s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Class (Def2 fake_no_def2));
        aux_toks xs

    | (T.Tvoid ii | T.Tint ii)
      ::T.TIdent (s, ii2)
      ::T.TOParen (ii3)
      ::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Method (Def2 fake_no_def2));
        aux_toks xs

    |   T.TIdent (s1, ii1)::T.TDot ii2
      ::T.TIdent (s3, ii3)::T.TIdent (s4,ii4)::xs 
       ->
        if not (Hashtbl.mem already_tagged ii4) && lexer_based_tagger
        then begin 
          tag ii4 (Field (Def2 fake_no_def2));

          tag ii3 (TypeMisc);
          if is_module_name s1 then tag ii1 (Module (Use));
        end;
        aux_toks xs
*)

    (* uses *)
(*
    |   T.TIdent (s1, ii1)::T.TDot ii2
      ::T.TIdent (s3, ii3)::T.TOParen(ii4)::xs ->
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

    |   T.TIdent (s1, ii1)::T.TDot ii2
      ::T.TIdent (s3, ii3)::T.TEq ii4::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then begin 
          tag ii3 (Field (Use2 fake_no_use2));
          if is_module_name s1 then tag ii1 (Module (Use))
        end;
        aux_toks xs


    |  T.TIdent (s1, ii1)::T.TDot ii2
     ::T.TIdent (s3, ii3)::T.TDot ii4::xs ->
        if not (Hashtbl.mem already_tagged ii1) && lexer_based_tagger
        then begin 
          if is_module_name s1 then tag ii1 (Module Use)
        end;
        aux_toks (T.TIdent (s3, ii3)::T.TDot ii4::xs)
*)        

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
        if not (Hashtbl.mem already_tagged ii) (* could be Estet tagged *)
        then tag ii Comment

    | T.TCommentSpace ii -> ()
    | T.TCommentNewline ii | T.TCommentMisc ii -> ()

    | T.TUnknown ii -> tag ii Error
    | T.EOF ii-> ()

    (* values  *)

    | T.TGUIL ii | T.T_ENCAPSED (_, ii) ->
        tag ii String
    | T.TFloat (s,ii) | T.TInt (s,ii) ->
        tag ii Number

    | T.TOp(_, ii) -> tag ii Operator

    (* keywords  *)
    | T.TIdent("bool", ii) -> tag ii TypeMisc

    | T.TIdent(("int" | "float"), ii) -> tag ii TypeInt
    | T.TIdent("string", ii) -> tag ii TypeMisc
    | T.Tint ii | T.Tfloat ii -> tag ii TypeInt
    | T.Tstring ii -> tag ii TypeMisc

    | T.Tpublic ii | T.Tprivate ii
        -> tag ii Keyword

    | T.Tmatch ii | T.Tcase ii | T.Tdefault ii
        -> tag ii KeywordConditional

    | T.Ttype ii
    | T.Twith ii
    | T.Tas ii
        -> tag ii Keyword

    | T.Tpackage ii | T.Tmodule ii
    | T.Timport ii
        -> tag ii KeywordModule

    | T.Tif ii | T.Tthen ii | T.Telse ii -> tag ii KeywordConditional
    | T.Tdo ii -> tag ii KeywordLoop

    | T.TIdent(("true" | "false"), ii) -> tag ii Boolean

    | T.Tclient ii
        -> tag ii Keyword

    | T.Tprotected ii
    | T.Texposed ii
    | T.Tforall ii
    | T.Texternal ii
    | T.Tserver ii
    | T.Tparser ii
    | T.Tdb ii
    | T.Tcss ii
    | T.Tend ii
    | T.Tbegin ii
    | T.Trec ii
    | T.Tand ii
    | T.Tval ii
    | T.Tor ii
    | T.Tfunction ii
        -> tag ii Keyword

    (* xml  *)
    | T.T_XML_OPEN_TAG (s, ii) ->
        (* todo: match s with ...
         * look the html highlighter in lang_html/ ?
         *)
        tag ii Keyword
    | T.T_XML_CLOSE_TAG (sopt, ii) ->
        (* todo: match s with ... *)
        tag ii Keyword

    | T.T_XML_ATTR (s, ii) ->
        (* todo: match s with ...
         * look the html highlighter in lang_html/ ?
         *)
        tag ii Keyword
    | T.T_XML_MORE ii ->
        tag ii Keyword
    | T.T_XML_TEXT (s, ii) -> tag ii String

    (* css *)
    | T.TSharpIdent (s, ii) ->
        ()

    | T.TSharp ii
        -> tag ii Punctuation

    (* symbols *)
    | T.TEq ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Punctuation

    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii
          -> tag ii Punctuation

    | T.TPlus ii | T.TMinus ii
    | T.TLess ii | T.TMore ii
        -> tag ii Operator

    | T.TDot (ii)
    | T.TColon (ii)
        -> tag ii Punctuation

    | T.TStar ii
      -> tag ii Operator

    | T.TAnd ii
        -> tag ii Operator

    | T.TNotEq ii
    | T.TEqEq ii

    | T.TXor ii
    | T.TOr ii
        -> tag ii Operator

    | T.TComma ii
        -> tag ii Punctuation

    | T.TDiv ii
        -> tag ii Operator

    | T.TArrow ii
    | T.TQuestion ii
    | T.TSemiColon ii
        -> tag ii Punctuation

    | T.THat ii
    | T.TOrOr ii
    | T.TAndAnd ii
    | T.TAntiSlash ii
    | T.TAt ii
    | T.TUnderscore ii
    | T.TTilde ii
        -> tag ii Punctuation

    | T.TExternalIdent (s, ii) ->
        ()
    | T.TIdent (s, ii) ->
        ()
  );
  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  

  ()
