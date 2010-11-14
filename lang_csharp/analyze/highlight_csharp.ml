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

open Ast_csharp

module Ast = Ast_csharp
(*module V = Visitor_csharp *)

open Highlight_code

module T = Parser_csharp
module TH = Token_helpers_csharp

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

    (* uses *)

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

    (* keywords  *)
    | T.Tbool ii
    | T.Tbyte ii
    | T.Tchar ii
    | T.Tvoid ii
    | T.Tdouble ii
    | T.Tfloat ii
    | T.Tshort ii
    | T.Tint ii
    | T.Tlong ii
    | T.Tstring ii
    | T.Tsbyte ii
    | T.Tushort ii
    | T.Tuint ii
    | T.Tulong ii
    | T.Tclass ii
    | T.Tabstract ii
    | T.Tvirtual ii
    | T.Tdelegate ii
    | T.Tthis ii
    | T.Tinterface ii
    | T.Tnew ii
    | T.Tobject ii
    | T.Tprivate ii
    | T.Tprotected ii
    | T.Tpublic ii
    | T.Treturn ii
    | T.Tbreak ii
    | T.Tcontinue ii
    | T.Tswitch ii
    | T.Tcase ii
    | T.Tdefault ii
    | T.Tenum ii
    | T.Tstruct ii
    | T.Tconst ii
    | T.Tunsafe ii
    | T.Tnamespace ii
    | T.Tusing ii
    | T.Tstatic ii
    | T.Tvolatile ii
    | T.Textern ii
    | T.Tif ii
    | T.Telse ii
    | T.Tdo ii
    | T.Twhile ii
    | T.Tfor ii
    | T.Tforeach ii
    | T.Tgoto ii
    | T.Tthrow ii
    | T.Ttry ii
    | T.Tcatch ii
    | T.Tfinally ii
    | T.Tchecked ii
    | T.Tunchecked ii
    | T.Tnull ii
    | T.Ttrue ii
    | T.Tfalse ii
    | T.Tref ii
    | T.Tout ii
    | T.Tas ii
    | T.Tbase ii
    | T.Tdecimal ii
    | T.Tevent ii
    | T.Texplicit ii
    | T.Tfixed ii
    | T.Timplicit ii
    | T.Tin ii
    | T.Tinternal ii
    | T.Tis ii
    | T.Tlock ii
    | T.Toperator ii
    | T.Toverride ii
    | T.Tparams ii
    | T.Treadonly ii
    | T.Tsealed ii
    | T.Tsizeof ii
    | T.Tstackalloc ii
    | T.Ttypeof ii
    | T.TCppLine ii
    | T.TCppError ii
    | T.TCppWarning ii
    | T.TCppRegion ii
    | T.TCppEndRegion ii
    | T.TDefine ii
    | T.TUndef ii
    | T.TIfdefIf ii
    | T.TIfdefElif ii
    | T.TIfdefElse ii
    | T.TIfdefEndif ii
        -> ()

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

    | T.TAnd ii
        -> tag ii Punctuation

    | T.TAssignOp (_, ii) -> tag ii Punctuation

    | T.TNotEq ii
    | T.TLessEq ii
    | T.TMoreEq ii
    | T.TEqEq ii
    | T.TXor ii
    | T.TOr ii
    | T.TPercent ii

    | T.TComma ii
    | T.TCAngle ii
    | T.TOAngle ii
        -> tag ii Punctuation

    | T.TArrow ii
    | T.TOrOr ii 
    | T.TAndAnd ii 
    | T.TBang ii 
    | T.TDec ii
    | T.TInc ii
    | T.TQuestion ii
    | T.TDiv ii
    | T.TSemiColon ii
        -> tag ii Punctuation

    | T.TIdent (s, ii) -> 
        ()
  );
  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  

  ()
