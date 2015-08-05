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

open Parser_python

module Ast = Ast_python
module PI = Parse_info

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
  | Tdef ii -> Tdef (f ii)
  | Tlambda ii -> Tlambda (f ii)
  | Tclass ii -> Tclass (f ii)
  | Tif ii -> Tif (f ii)
  | Telse ii -> Telse (f ii)
  | Telif ii -> Telif (f ii)
  | Twhile ii -> Twhile (f ii)
  | Tfor ii -> Tfor (f ii)
  | Treturn ii -> Treturn (f ii)
  | Tyield ii -> Tyield (f ii)
  | Tbreak ii -> Tbreak (f ii)
  | Tcontinue ii -> Tcontinue (f ii)
  | Ttry ii -> Ttry (f ii)
  | Traise ii -> Traise (f ii)
  | Tfinally ii -> Tfinally (f ii)
  | Tor ii -> Tor (f ii)
  | Tand ii -> Tand (f ii)
  | Tnot ii -> Tnot (f ii)
  | Tglobal ii -> Tglobal (f ii)
  | Tdel ii -> Tdel (f ii)
  | Tfrom ii -> Tfrom (f ii)
  | Tas ii -> Tas (f ii)
  | Twith ii -> Twith (f ii)
  | Tassert ii -> Tassert (f ii)
  | Tpass ii -> Tpass (f ii)
  | Texcept ii -> Texcept (f ii)
  | Timport ii -> Timport (f ii)
  | Tprint ii -> Tprint (f ii)
  | Texec ii -> Texec (f ii)
  | Tin ii -> Tin (f ii)
  | Tis ii -> Tis (f ii)

  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)

  | TInt (s, ii) -> TInt (s, f ii)
  | TFloat (s, ii) -> TFloat (s, f ii)
  | TComplex (s, ii) -> TComplex (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)
  | TString (s, ii) -> TString (s, f ii)
  | TLongString (s, ii) -> TLongString (s, f ii)

  | TIdent (s, ii) -> TIdent (s, f ii)

  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBracket ii -> TOBracket (f ii)
  | TCBracket ii -> TCBracket (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)
  | TOAngle ii -> TOAngle (f ii)
  | TCAngle ii -> TCAngle (f ii)
  | TComma ii -> TComma (f ii)
  | TColon ii -> TColon (f ii)
  | TBackQuote ii -> TBackQuote (f ii)
  | TDot ii -> TDot (f ii)
  | TEllipsis ii -> TEllipsis (f ii)
  | TStar ii -> TStar (f ii)
  | TStarStar ii -> TStarStar (f ii)
  | TEq ii -> TEq (f ii)
  | TMinus ii -> TMinus (f ii)
  | TPlus ii -> TPlus (f ii)
  | TTilde ii -> TTilde (f ii)
  | TSlash ii -> TSlash (f ii)
  | TSlashSlash ii -> TSlashSlash (f ii)
  | TPercent ii -> TPercent (f ii)
  | TAnd ii -> TAnd (f ii)
  | TOr ii -> TOr (f ii)
  | TXor ii -> TXor (f ii)
  | TLess ii -> TLess (f ii)
  | TMore ii -> TMore (f ii)
  | TEqEq ii -> TEqEq (f ii)
  | TMoreEq ii -> TMoreEq (f ii)
  | TLessEq ii -> TLessEq (f ii)
  | TDiff ii -> TDiff (f ii)
  | TNotEq ii -> TNotEq (f ii)
  | TAt ii -> TAt (f ii)
  | TAugOp (s, ii) -> TAugOp (s, f ii)

let info_of_tok tok = 
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok +> ignore;
  Common2.some !res

