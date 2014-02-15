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

open Parser_opa

module Ast = Ast_opa
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _ | TCommentSpace _ | TCommentNewline _ -> true
  | TCommentMisc _ -> true
  | _ -> false 

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  | TComment ii -> TComment (f ii)
  | TCommentMisc ii -> TCommentMisc (f ii)

  | TTilde ii -> TTilde (f ii)
  | TGUIL ii -> TGUIL (f ii)
  | T_ENCAPSED (s, ii) -> T_ENCAPSED (s, f ii)
  | T_XML_OPEN_TAG (s, ii) -> T_XML_OPEN_TAG (s, f ii)
  | T_XML_CLOSE_TAG (s, ii) -> T_XML_CLOSE_TAG (s, f ii)
  | T_XML_ATTR (s, ii) -> T_XML_ATTR (s, f ii)
  | T_XML_MORE (ii) -> T_XML_MORE (f ii)
  | T_XML_SLASH_GT (ii) -> T_XML_SLASH_GT (f ii)
  | T_XML_TEXT (s, ii) -> T_XML_TEXT (s, f ii)

  | T_CSS_TEXT (ii) -> T_CSS_TEXT(f ii)
  | T_PARSER_BEFORE_ARROW ii -> T_PARSER_BEFORE_ARROW (f ii)

  | TInt (s, ii) -> TInt (s, f ii)
  | TFloat (s, ii) -> TFloat (s, f ii)
  | TIdent (s, ii) -> TIdent (s, f ii)
  | TTypeVar (s, ii) -> TTypeVar (s, f ii)
  | TExternalIdent (s, ii) -> TExternalIdent (s, f ii)
  | TOp (s, ii) -> TOp (s, f ii)
  | TSharpIdent (s, ii) -> TSharpIdent (s, f ii)


  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)

  | TComma ii -> TComma (f ii)
  | TColon ii -> TColon (f ii)
  | TDot ii -> TDot (f ii)
  | TSemiColon ii -> TSemiColon (f ii)
  | TStar ii -> TStar (f ii)
  | TDiv ii -> TDiv (f ii)

  | TEq ii -> TEq (f ii)

  | TNotEq ii -> TNotEq (f ii)
  | TPlus ii -> TPlus (f ii)
  | TMinus ii -> TMinus (f ii)

  | TAnd ii -> TAnd (f ii)
  | TOr ii -> TOr (f ii)
  | TXor ii -> TXor (f ii)
  | TLess ii -> TLess (f ii)
  | TMore ii -> TMore (f ii)

  | TQuestion ii -> TQuestion (f ii)
  | TAt ii -> TAt (f ii)
  | TUnderscore ii -> TUnderscore (f ii)

  | Tpackage ii -> Tpackage (f ii)
  | Timport ii -> Timport (f ii)
  | Tforall ii -> Tforall (f ii)
  | Texternal ii -> Texternal (f ii)
  | Tserver ii -> Tserver (f ii)
  | Tparser ii -> Tparser (f ii)
  | Tdatabase ii -> Tdatabase (f ii)
  | Tcss ii -> Tcss (f ii)
  | Tend ii -> Tend (f ii)
  | Tbegin ii -> Tbegin (f ii)
  | Trec ii -> Trec (f ii)
  | Tand ii -> Tand (f ii)
  | Tval ii -> Tval (f ii)
  | Ttype ii -> Ttype (f ii)
  | Tdo ii -> Tdo (f ii)
  | Tas ii -> Tas (f ii)
  | Twith ii -> Twith (f ii)
  | Tmatch ii -> Tmatch (f ii)
  | Telse ii -> Telse (f ii)
  | Tthen ii -> Tthen (f ii)
  | Tif ii -> Tif (f ii)

  | Tint ii -> Tint (f ii)
  | Tstring ii -> Tstring (f ii)
  | Tfloat ii -> Tfloat (f ii)

  | Tprotected ii -> Tprotected (f ii)
  | Texposed ii -> Texposed (f ii)
  | Tclient ii -> Tclient (f ii)
  | Tprivate ii -> Tprivate (f ii)
  | Tpublic ii -> Tpublic (f ii)
  | Tmodule ii -> Tmodule (f ii)
  | Tor ii -> Tor (f ii)
  | Tfunction ii -> Tfunction (f ii)
  | Tdefault ii -> Tdefault (f ii)
  | Tcase ii -> Tcase (f ii)

  | THat ii -> THat (f ii)
  | TSharp ii -> TSharp (f ii)
  | TAntiSlash ii -> TAntiSlash (f ii)
  | TEqEq ii -> TEqEq (f ii)

  | TAndAnd ii -> TAndAnd (f ii)
  | TOrOr ii -> TOrOr (f ii)
  | TArrow ii -> TArrow (f ii)


  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)


let info_of_tok tok = 
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok +> ignore;
  Common2.some !res

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let linecol_of_tok tok =
  let info = info_of_tok tok in
  PI.line_of_info info, PI.col_of_info info

let line_of_tok x = fst (linecol_of_tok x)

let str_of_tok  x = PI.str_of_info  (info_of_tok x)
let file_of_tok x = PI.file_of_info (info_of_tok x)
let pos_of_tok x =  PI.pos_of_info (info_of_tok x)

