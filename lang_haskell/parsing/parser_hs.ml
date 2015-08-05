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
  | TComment of (Parse_info.info)
  | TCommentSpace of (Parse_info.info)
  | TCommentNewline of (Parse_info.info)

  | TNumber of (string * Parse_info.info)
  | TString of (string * Parse_info.info)
  | TChar of (string * Parse_info.info)

  | TIdent of (string * Parse_info.info)
  | TUpperIdent of (string * Parse_info.info)
  | TSymbol of (string * Parse_info.info)

  | TOParen of (Parse_info.info)
  | TCParen of (Parse_info.info)
  | TOBracket of (Parse_info.info)
  | TCBracket of (Parse_info.info)
  | TOBrace of (Parse_info.info)
  | TCBrace of (Parse_info.info)

  | TComma of (Parse_info.info)
  | TSemiColon of (Parse_info.info)
  | TPipe of (Parse_info.info)

  | Tdata of (Parse_info.info)
  | Tnewtype of (Parse_info.info)
  | Ttype of (Parse_info.info)

  | Tclass of (Parse_info.info)
  | Tinstance of (Parse_info.info)
  | Tdefault of (Parse_info.info)
  | Tderiving of (Parse_info.info)
  | Tdo of (Parse_info.info)
  | Tif of (Parse_info.info)
  | Tthen of (Parse_info.info)
  | Telse of (Parse_info.info)
  | Tcase of (Parse_info.info)
  | Tof of (Parse_info.info)
  | Tmodule of (Parse_info.info)
  | Timport of (Parse_info.info)
  | Tlet of (Parse_info.info)
  | Tin of (Parse_info.info)
  | Twhere of (Parse_info.info)
  | Tinfix of (Parse_info.info)
  | Tinfixl of (Parse_info.info)
  | Tinfixr of (Parse_info.info)

  | Tqualified of (Parse_info.info)
  | Tas of (Parse_info.info)
  | Thiding of (Parse_info.info)

  | TUnknown of (Parse_info.info)
  | EOF of (Parse_info.info)

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
