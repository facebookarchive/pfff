(* Copyright (C) 2008 Yoann Padioleau *)

module PI = Parse_info
open Parser_java

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _ | TCommentSpace _  | TCommentNewline _ -> true
  | _ -> false 

let is_just_comment = function
  | TComment _ -> true
  | _ -> false 

let token_kind_of_tok t =
  match t with
  | LC _ -> PI.LBrace
  | RC _ -> PI.RBrace
  | LP _ -> PI.LPar
  | RP _ -> PI.RPar
  | TComment _ -> PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

(* Because ocamlyacc force us to do it that way. The ocamlyacc token 
 * cant be a pair of a sum type, it must be directly a sum type.
 *)
let info_of_tok = function
  | TUnknown ii -> ii
  | TComment ii -> ii
  | TCommentSpace ii -> ii
  | TCommentNewline ii -> ii

  | TInt (_s, ii) -> ii
  | TFloat (_s, ii) -> ii
  | TChar (_s, ii) -> ii
  | TString (_s, ii) -> ii

  | IDENTIFIER (_id,ii) -> ii
  | PRIMITIVE_TYPE (_s, ii) -> ii
  | LITERAL (_s, ii) -> ii

  | OPERATOR_EQ (_op, ii) -> ii

  (* 3.11 Separators *)
  | LP (ii) -> ii
  | RP (ii) -> ii
  | LC (ii) -> ii
  | RC (ii) -> ii
  | LB (ii) -> ii
  | RB (ii) -> ii
  | SM (ii) -> ii
  | CM (ii) -> ii
  | DOT (ii) -> ii

  | LB_RB (ii) -> ii

  (* 3.12 Operators *)
  | EQ (ii) -> ii
  | GT (ii) -> ii
  | LT (ii) -> ii
  | LT2 (ii) -> ii
  | NOT (ii) -> ii
  | COMPL (ii) -> ii
  | COND (ii) -> ii
  | COLON (ii) -> ii
  | EQ_EQ (ii) -> ii
  | LE (ii) -> ii
  | GE (ii) -> ii
  | NOT_EQ (ii) -> ii
  | AND_AND (ii) -> ii
  | OR_OR (ii) -> ii
  | INCR (ii) -> ii
  | DECR (ii) -> ii
  | PLUS (ii) -> ii
  | MINUS (ii) -> ii
  | TIMES (ii) -> ii
  | DIV (ii) -> ii
  | AND (ii) -> ii
  | OR (ii) -> ii
  | XOR (ii) -> ii
  | MOD (ii) -> ii
  | LS (ii) -> ii
  | SRS (ii) -> ii
  | URS (ii) -> ii

  | AT (ii) -> ii
  | DOTS (ii) -> ii

  | ABSTRACT (ii) -> ii
  | BOOLEAN (ii) -> ii
  | BREAK (ii) -> ii
  | BYTE (ii) -> ii
  | CASE (ii) -> ii
  | CATCH (ii) -> ii
  | CHAR (ii) -> ii
  | CLASS (ii) -> ii
  | CONST (ii) -> ii
  | CONTINUE (ii) -> ii
  | DEFAULT (ii) -> ii
  | DO (ii) -> ii
  | DOUBLE (ii) -> ii
  | ELSE (ii) -> ii
  | EXTENDS (ii) -> ii
  | FINAL (ii) -> ii
  | FINALLY (ii) -> ii
  | FLOAT (ii) -> ii
  | FOR (ii) -> ii
  | GOTO (ii) -> ii
  | IF (ii) -> ii
  | IMPLEMENTS (ii) -> ii
  | IMPORT (ii) -> ii
  | INSTANCEOF (ii) -> ii
  | INT (ii) -> ii
  | INTERFACE (ii) -> ii
  | LONG (ii) -> ii
  | NATIVE (ii) -> ii
  | NEW (ii) -> ii
  | PACKAGE (ii) -> ii
  | PRIVATE (ii) -> ii
  | PROTECTED (ii) -> ii
  | PUBLIC (ii) -> ii
  | RETURN (ii) -> ii
  | SHORT (ii) -> ii
  | STATIC (ii) -> ii
  | STRICTFP (ii) -> ii
  | SUPER (ii) -> ii
  | SWITCH (ii) -> ii
  | SYNCHRONIZED (ii) -> ii
  | THIS (ii) -> ii
  | THROW (ii) -> ii
  | THROWS (ii) -> ii
  | TRANSIENT (ii) -> ii
  | TRY (ii) -> ii
  | VOID (ii) -> ii
  | VOLATILE (ii) -> ii
  | WHILE (ii) -> ii

  | ASSERT (ii) -> ii
  | ENUM (ii) -> ii

  | EOF (ii) -> ii

(* used by tokens to complete the parse_info with filename, line, col infos *)
let visitor_info_of_tok f = function

  | TUnknown ii -> TUnknown (f ii)
  | TComment ii -> TComment (f ii)
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)

  | TInt (s, ii) -> TInt (s, f ii)
  | TFloat (s, ii) -> TFloat (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)
  | TString (s, ii) -> TString (s, f ii)

  | IDENTIFIER (id,ii) -> IDENTIFIER (id, f ii)
  | PRIMITIVE_TYPE (s, ii) -> PRIMITIVE_TYPE (s, f ii)
  | LITERAL (s, ii) -> LITERAL (s, f ii)

  | OPERATOR_EQ (op, ii) -> OPERATOR_EQ (op, f ii)

  (* 3.11 Separators *)
  | LP (ii) -> LP (f ii)
  | RP (ii) -> RP (f ii)
  | LC (ii) -> LC (f ii)
  | RC (ii) -> RC (f ii)
  | LB (ii) -> LB (f ii)
  | LB_RB (ii) -> LB_RB (f ii)
  | RB (ii) -> RB (f ii)
  | SM (ii) -> SM (f ii)
  | CM (ii) -> CM (f ii)
  | DOT (ii) -> DOT (f ii)

  (* 3.12 Operators *)
  | EQ (ii) -> EQ (f ii)
  | GT (ii) -> GT (f ii)
  | LT (ii) -> LT (f ii)
  | LT2 (ii) -> LT2 (f ii)
  | NOT (ii) -> NOT (f ii)
  | COMPL (ii) -> COMPL (f ii)
  | COND (ii) -> COND (f ii)
  | COLON (ii) -> COLON (f ii)
  | EQ_EQ (ii) -> EQ_EQ (f ii)
  | LE (ii) -> LE (f ii)
  | GE (ii) -> GE (f ii)
  | NOT_EQ (ii) -> NOT_EQ (f ii)
  | AND_AND (ii) -> AND_AND (f ii)
  | OR_OR (ii) -> OR_OR (f ii)
  | INCR (ii) -> INCR (f ii)
  | DECR (ii) -> DECR (f ii)
  | PLUS (ii) -> PLUS (f ii)
  | MINUS (ii) -> MINUS (f ii)
  | TIMES (ii) -> TIMES (f ii)
  | DIV (ii) -> DIV (f ii)
  | AND (ii) -> AND (f ii)
  | OR (ii) -> OR (f ii)
  | XOR (ii) -> XOR (f ii)
  | MOD (ii) -> MOD (f ii)
  | LS (ii) -> LS (f ii)
  | SRS (ii) -> SRS (f ii)
  | URS (ii) -> URS (f ii)

  | AT (ii) -> AT (f ii)
  | DOTS (ii) -> DOTS (f ii)

  | ABSTRACT (ii) -> ABSTRACT (f ii)
  | BOOLEAN (ii) -> BOOLEAN (f ii)
  | BREAK (ii) -> BREAK (f ii)
  | BYTE (ii) -> BYTE (f ii)
  | CASE (ii) -> CASE (f ii)
  | CATCH (ii) -> CATCH (f ii)
  | CHAR (ii) -> CHAR (f ii)
  | CLASS (ii) -> CLASS (f ii)
  | CONST (ii) -> CONST (f ii)
  | CONTINUE (ii) -> CONTINUE (f ii)
  | DEFAULT (ii) -> DEFAULT (f ii)
  | DO (ii) -> DO (f ii)
  | DOUBLE (ii) -> DOUBLE (f ii)
  | ELSE (ii) -> ELSE (f ii)
  | EXTENDS (ii) -> EXTENDS (f ii)
  | FINAL (ii) -> FINAL (f ii)
  | FINALLY (ii) -> FINALLY (f ii)
  | FLOAT (ii) -> FLOAT (f ii)
  | FOR (ii) -> FOR (f ii)
  | GOTO (ii) -> GOTO (f ii)
  | IF (ii) -> IF (f ii)
  | IMPLEMENTS (ii) -> IMPLEMENTS (f ii)
  | IMPORT (ii) -> IMPORT (f ii)
  | INSTANCEOF (ii) -> INSTANCEOF (f ii)
  | INT (ii) -> INT (f ii)
  | INTERFACE (ii) -> INTERFACE (f ii)
  | LONG (ii) -> LONG (f ii)
  | NATIVE (ii) -> NATIVE (f ii)
  | NEW (ii) -> NEW (f ii)
  | PACKAGE (ii) -> PACKAGE (f ii)
  | PRIVATE (ii) -> PRIVATE (f ii)
  | PROTECTED (ii) -> PROTECTED (f ii)
  | PUBLIC (ii) -> PUBLIC (f ii)
  | RETURN (ii) -> RETURN (f ii)
  | SHORT (ii) -> SHORT (f ii)
  | STATIC (ii) -> STATIC (f ii)
  | STRICTFP (ii) -> STRICTFP (f ii)
  | SUPER (ii) -> SUPER (f ii)
  | SWITCH (ii) -> SWITCH (f ii)
  | SYNCHRONIZED (ii) -> SYNCHRONIZED (f ii)
  | THIS (ii) -> THIS (f ii)
  | THROW (ii) -> THROW (f ii)
  | THROWS (ii) -> THROWS (f ii)
  | TRANSIENT (ii) -> TRANSIENT (f ii)
  | TRY (ii) -> TRY (f ii)
  | VOID (ii) -> VOID (f ii)
  | VOLATILE (ii) -> VOLATILE (f ii)
  | WHILE (ii) -> WHILE (f ii)

  | ASSERT (ii) -> ASSERT (f ii)
  | ENUM (ii) -> ENUM (f ii)

  | EOF (ii) -> EOF (f ii)

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* todo: remove, just use by parse_java.ml checkpoint mechanism that
 * we actually don't use
 *)
let line_of_tok tok =
  let info = info_of_tok tok in
  PI.line_of_info info
