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

module PI = Parse_info

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type token =
  | TComment of (Ast_hs.info)
  | TCommentSpace of (Ast_hs.info)
  | TCommentNewline of (Ast_hs.info)

  | TNumber of (string * Ast_hs.info)
  | TString of (string * Ast_hs.info)
  | TChar of (string * Ast_hs.info)

  | TIdent of (string * Ast_hs.info)
  | TUpperIdent of (string * Ast_hs.info)
  | TSymbol of (string * Ast_hs.info)

  | TOParen of (Ast_hs.info)
  | TCParen of (Ast_hs.info)
  | TOBracket of (Ast_hs.info)
  | TCBracket of (Ast_hs.info)
  | TOBrace of (Ast_hs.info)
  | TCBrace of (Ast_hs.info)

  | TComma of (Ast_hs.info)
  | TSemiColon of (Ast_hs.info)
  | TPipe of (Ast_hs.info)

  | Tdata of (Ast_hs.info)
  | Tnewtype of (Ast_hs.info)
  | Ttype of (Ast_hs.info)

  | Tclass of (Ast_hs.info)
  | Tinstance of (Ast_hs.info)
  | Tdefault of (Ast_hs.info)
  | Tderiving of (Ast_hs.info)
  | Tdo of (Ast_hs.info)
  | Tif of (Ast_hs.info)
  | Tthen of (Ast_hs.info)
  | Telse of (Ast_hs.info)
  | Tcase of (Ast_hs.info)
  | Tof of (Ast_hs.info)
  | Tmodule of (Ast_hs.info)
  | Timport of (Ast_hs.info)
  | Tlet of (Ast_hs.info)
  | Tin of (Ast_hs.info)
  | Twhere of (Ast_hs.info)
  | Tinfix of (Ast_hs.info)
  | Tinfixl of (Ast_hs.info)
  | Tinfixr of (Ast_hs.info)

  | Tqualified of (Ast_hs.info)
  | Tas of (Ast_hs.info)
  | Thiding of (Ast_hs.info)

  | TUnknown of (Ast_hs.info)
  | EOF of (Ast_hs.info)

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _ | TCommentSpace _ | TCommentNewline _ -> true
  | _ -> false 

let is_just_comment = function
  | TComment _ -> true
  | _ -> false 

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)
let visitor_info_of_tok f = function
  | TComment ii -> TComment (f ii)
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)

  | TNumber (s, ii) -> TNumber (s, f ii)
  | TString (s, ii) -> TString (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)

  | TIdent (s, ii) -> TIdent (s, f ii)
  | TUpperIdent (s, ii) -> TUpperIdent (s, f ii)
  | TSymbol (s, ii) -> TSymbol (s, f ii)

  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)

  | TPipe ii -> TPipe (f ii)
  | TSemiColon ii -> TSemiColon (f ii)

  | TComma ii -> TComma (f ii)

  | Tdata ii -> Tdata (f ii)
  | Tnewtype ii -> Tnewtype (f ii)
  | Ttype ii -> Ttype (f ii)
  | Tclass ii -> Tclass (f ii)
  | Tinstance ii -> Tinstance (f ii)
  | Tdefault ii -> Tdefault (f ii)
  | Tderiving ii -> Tderiving (f ii)
  | Tdo ii -> Tdo (f ii)
  | Tif ii -> Tif (f ii)
  | Tthen ii -> Tthen (f ii)
  | Telse ii -> Telse (f ii)
  | Tcase ii -> Tcase (f ii)
  | Tof ii -> Tof (f ii)
  | Tmodule ii -> Tmodule (f ii)
  | Timport ii -> Timport (f ii)
  | Tlet ii -> Tlet (f ii)
  | Tin ii -> Tin (f ii)
  | Twhere ii -> Twhere (f ii)
  | Tinfix ii -> Tinfix (f ii)
  | Tinfixl ii -> Tinfixl (f ii)
  | Tinfixr ii -> Tinfixr (f ii)

  | Tqualified ii -> Tqualified (f ii)
  | Tas ii -> Tas (f ii)
  | Thiding ii -> Thiding (f ii)


  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)

let info_of_tok tok = 
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok +> ignore;
  Common2.some !res


let str_of_tok  x = PI.str_of_info  (info_of_tok x)
