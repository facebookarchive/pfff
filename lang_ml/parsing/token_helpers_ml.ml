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

open Parser_ml
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

(*
let is_just_comment = function
  | TComment _ -> true
  | _ -> false 
*)

let token_kind_of_tok t =
  match t with
  | TOBrace _ -> PI.LBrace
  | TCBrace _ -> PI.RBrace
  | TOParen _ -> PI.LPar
  | TCParen _ -> PI.RPar
  | TComment _ | TCommentMisc _ -> PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)
let info_of_tok = function

  | TCommentSpace ii -> ii
  | TCommentNewline ii -> ii
  | TComment ii -> ii
  | TCommentMisc ii -> ii
  | TUnknown ii -> ii
  | EOF ii -> ii

  | TSharpDirective ii -> ii

  | TInt  (_s, ii) -> ii
  | TFloat  (_s, ii) -> ii
  | TChar  (_s, ii) -> ii
  | TString  (_s, ii) -> ii
  | TLowerIdent  (_s, ii) -> ii
  | TUpperIdent  (_s, ii) -> ii
  | TLabelUse  (_s, ii) -> ii
  | TLabelDecl  (_s, ii) -> ii
  | TOptLabelUse  (_s, ii) -> ii
  | TOptLabelDecl  (_s, ii) -> ii

  | TPrefixOperator (_s, ii) -> ii
  | TInfixOperator (_s, ii) -> ii

  | Tfun ii -> ii
  | Tfunction ii -> ii
  | Trec ii -> ii
  | Ttype ii -> ii
  | Tof ii -> ii
  | Tif ii -> ii
  | Tthen ii -> ii
  | Telse ii -> ii
  | Tmatch ii -> ii
  | Twith ii -> ii
  | Twhen ii -> ii
  | Tlet ii -> ii
  | Tin ii -> ii
  | Tas ii -> ii
  | Ttry ii -> ii
  | Texception ii -> ii
  | Tbegin ii -> ii
  | Tend ii -> ii
  | Tfor ii -> ii
  | Tdo ii -> ii
  | Tdone ii -> ii
  | Tdownto ii -> ii
  | Twhile ii -> ii
  | Tto ii -> ii
  | Tval ii -> ii
  | Texternal ii -> ii
  | Ttrue ii -> ii
  | Tfalse ii -> ii
  | Tmodule ii -> ii
  | Topen ii -> ii
  | Tfunctor ii -> ii
  | Tinclude ii -> ii
  | Tsig ii -> ii
  | Tstruct ii -> ii
  | Tclass ii -> ii
  | Tnew ii -> ii
  | Tinherit ii -> ii
  | Tconstraint ii -> ii
  | Tinitializer ii -> ii
  | Tmethod ii -> ii
  | Tobject ii -> ii
  | Tprivate ii -> ii
  | Tvirtual ii -> ii
  | Tlazy ii -> ii
  | Tmutable ii -> ii
  | Tassert ii -> ii
  | Tand ii -> ii
  | Tor ii -> ii
  | Tmod ii -> ii
  | Tlor ii -> ii
  | Tlsl ii -> ii
  | Tlsr ii -> ii
  | Tlxor ii -> ii
  | Tasr ii -> ii
  | Tland ii -> ii
  | TOParen ii -> ii
  | TCParen ii -> ii
  | TOBrace ii -> ii
  | TCBrace ii -> ii
  | TOBracket ii -> ii
  | TCBracket ii -> ii
  | TOBracketPipe ii -> ii
  | TPipeCBracket ii -> ii
  | TOBracketLess ii -> ii
  | TGreaterCBracket ii -> ii
  | TOBraceLess ii -> ii
  | TGreaterCBrace ii -> ii
  | TOBracketGreater ii -> ii
  | TColonGreater ii -> ii
  | TLess ii -> ii
  | TGreater ii -> ii
  | TDot ii -> ii
  | TDotDot ii -> ii
  | TComma ii -> ii
  | TEq ii -> ii
  | TAssign ii -> ii
  | TAssignMutable ii -> ii
  | TColon ii -> ii
  | TColonColon ii -> ii
  | TBang ii -> ii
  | TBangEq ii -> ii
  | TTilde ii -> ii
  | TPipe ii -> ii
  | TSemiColon ii -> ii
  | TSemiColonSemiColon ii -> ii
  | TQuestion ii -> ii
  | TQuestionQuestion ii -> ii
  | TUnderscore ii -> ii
  | TStar ii -> ii
  | TArrow ii -> ii
  | TQuote ii -> ii
  | TBackQuote ii -> ii
  | TAnd ii -> ii
  | TAndAnd ii -> ii
  | TSharp ii -> ii
  | TMinusDot ii -> ii
  | TPlusDot ii -> ii
  | TPlus ii -> ii
  | TMinus ii -> ii


let visitor_info_of_tok f = function
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  | TComment ii -> TComment (f ii)
  | TCommentMisc ii -> TCommentMisc (f ii)
  | TUnknown ii -> TUnknown (f ii)
  | EOF (ii) -> EOF (f ii)

  | TSharpDirective ii -> TSharpDirective (f ii)

  | TInt (s, ii) -> TInt (s, f ii)
  | TFloat (s, ii) -> TFloat (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)
  | TString (s, ii) -> TString (s, f ii)
  | TLowerIdent (s, ii) -> TLowerIdent (s, f ii)
  | TUpperIdent (s, ii) -> TUpperIdent (s, f ii)
  | TLabelUse (s, ii) -> TLabelUse (s, f ii)
  | TLabelDecl (s, ii) -> TLabelDecl (s, f ii)
  | TOptLabelUse (s, ii) -> TOptLabelUse (s, f ii)
  | TOptLabelDecl (s, ii) -> TOptLabelDecl (s, f ii)
  | TPrefixOperator (s, ii) -> TPrefixOperator (s, f ii)
  | TInfixOperator (s, ii) -> TInfixOperator (s, f ii)

  | Tfun (ii) -> Tfun (f ii)
  | Tfunction (ii) -> Tfunction (f ii)
  | Trec (ii) -> Trec (f ii)
  | Ttype (ii) -> Ttype (f ii)
  | Tof (ii) -> Tof (f ii)
  | Tif (ii) -> Tif (f ii)
  | Tthen (ii) -> Tthen (f ii)
  | Telse (ii) -> Telse (f ii)
  | Tmatch (ii) -> Tmatch (f ii)
  | Twith (ii) -> Twith (f ii)
  | Twhen (ii) -> Twhen (f ii)
  | Tlet (ii) -> Tlet (f ii)
  | Tin (ii) -> Tin (f ii)
  | Tas (ii) -> Tas (f ii)
  | Ttry (ii) -> Ttry (f ii)
  | Texception (ii) -> Texception (f ii)
  | Tbegin (ii) -> Tbegin (f ii)
  | Tend (ii) -> Tend (f ii)
  | Tfor (ii) -> Tfor (f ii)
  | Tdo (ii) -> Tdo (f ii)
  | Tdone (ii) -> Tdone (f ii)
  | Tdownto (ii) -> Tdownto (f ii)
  | Twhile (ii) -> Twhile (f ii)
  | Tto (ii) -> Tto (f ii)
  | Tval (ii) -> Tval (f ii)
  | Texternal (ii) -> Texternal (f ii)
  | Ttrue (ii) -> Ttrue (f ii)
  | Tfalse (ii) -> Tfalse (f ii)
  | Tmodule (ii) -> Tmodule (f ii)
  | Topen (ii) -> Topen (f ii)
  | Tfunctor (ii) -> Tfunctor (f ii)
  | Tinclude (ii) -> Tinclude (f ii)
  | Tsig (ii) -> Tsig (f ii)
  | Tstruct (ii) -> Tstruct (f ii)
  | Tclass (ii) -> Tclass (f ii)
  | Tnew (ii) -> Tnew (f ii)
  | Tinherit (ii) -> Tinherit (f ii)
  | Tconstraint (ii) -> Tconstraint (f ii)
  | Tinitializer (ii) -> Tinitializer (f ii)
  | Tmethod (ii) -> Tmethod (f ii)
  | Tobject (ii) -> Tobject (f ii)
  | Tprivate (ii) -> Tprivate (f ii)
  | Tvirtual (ii) -> Tvirtual (f ii)
  | Tlazy (ii) -> Tlazy (f ii)
  | Tmutable (ii) -> Tmutable (f ii)
  | Tassert (ii) -> Tassert (f ii)
  | Tand (ii) -> Tand (f ii)
  | Tor (ii) -> Tor (f ii)
  | Tmod (ii) -> Tmod (f ii)
  | Tlor (ii) -> Tlor (f ii)
  | Tlsl (ii) -> Tlsl (f ii)
  | Tlsr (ii) -> Tlsr (f ii)
  | Tlxor (ii) -> Tlxor (f ii)
  | Tasr (ii) -> Tasr (f ii)
  | Tland (ii) -> Tland (f ii)
  | TOParen (ii) -> TOParen (f ii)
  | TCParen (ii) -> TCParen (f ii)
  | TOBrace (ii) -> TOBrace (f ii)
  | TCBrace (ii) -> TCBrace (f ii)
  | TOBracket (ii) -> TOBracket (f ii)
  | TCBracket (ii) -> TCBracket (f ii)
  | TOBracketPipe (ii) -> TOBracketPipe (f ii)
  | TPipeCBracket (ii) -> TPipeCBracket (f ii)
  | TOBracketLess (ii) -> TOBracketLess (f ii)
  | TGreaterCBracket (ii) -> TGreaterCBracket (f ii)
  | TOBraceLess (ii) -> TOBraceLess (f ii)
  | TGreaterCBrace (ii) -> TGreaterCBrace (f ii)
  | TOBracketGreater (ii) -> TOBracketGreater (f ii)
  | TColonGreater (ii) -> TColonGreater (f ii)
  | TLess (ii) -> TLess (f ii)
  | TGreater (ii) -> TGreater (f ii)
  | TDot (ii) -> TDot (f ii)
  | TDotDot (ii) -> TDotDot (f ii)
  | TComma (ii) -> TComma (f ii)
  | TEq (ii) -> TEq (f ii)
  | TAssign (ii) -> TAssign (f ii)
  | TAssignMutable (ii) -> TAssignMutable (f ii)
  | TColon (ii) -> TColon (f ii)
  | TColonColon (ii) -> TColonColon (f ii)
  | TBang (ii) -> TBang (f ii)
  | TBangEq (ii) -> TBangEq (f ii)
  | TTilde (ii) -> TTilde (f ii)
  | TPipe (ii) -> TPipe (f ii)
  | TSemiColon (ii) -> TSemiColon (f ii)
  | TSemiColonSemiColon (ii) -> TSemiColonSemiColon (f ii)
  | TQuestion (ii) -> TQuestion (f ii)
  | TQuestionQuestion (ii) -> TQuestionQuestion (f ii)
  | TUnderscore (ii) -> TUnderscore (f ii)
  | TStar (ii) -> TStar (f ii)
  | TArrow (ii) -> TArrow (f ii)
  | TQuote (ii) -> TQuote (f ii)
  | TBackQuote (ii) -> TBackQuote (f ii)
  | TAnd (ii) -> TAnd (f ii)
  | TAndAnd (ii) -> TAndAnd (f ii)
  | TSharp (ii) -> TSharp (f ii)
  | TMinusDot (ii) -> TMinusDot (f ii)
  | TPlusDot (ii) -> TPlusDot (f ii)
  | TPlus (ii) -> TPlus (f ii)
  | TMinus (ii) -> TMinus (f ii)

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let line_of_tok tok = 
  let info = info_of_tok tok in
  PI.line_of_info info
