(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Poor's man clang AST (actually just for the C subset of clang).
 * 
 * related work:
 *  - https://github.com/facebook/facebook-clang-plugins
 *  - https://github.com/Antique-team/clangml
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
(* to report analysis error, need the .clang (or .clang2) filename and line *)
type loc = Common.filename * int

(* coupling: modify also parse_clang conv function *)
type enum = 

  | TodoAst of string
(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Comments *)
(* ------------------------------------------------------------------------- *)

 | FullComment
 | TextComment
 | ParagraphComment
 | InlineCommandComment
 | VerbatimLineComment
 | BlockCommandComment

(* ------------------------------------------------------------------------- *)
(* Attributes *)
(* ------------------------------------------------------------------------- *)

 | VisibilityAttr
 | DeprecatedAttr
 | MaxFieldAlignmentAttr
 | AlwaysInlineAttr
 | NoDebugAttr
 | ConstAttr
 | NoThrowAttr
 | NonNullAttr
 | AsmLabelAttr
 | PackedAttr
 | FormatAttr
 | AlignedAttr
 | WarnUnusedResultAttr
 | MayAliasAttr
 | PureAttr
 | MallocAttr
 | ReturnsTwiceAttr
 | UnusedAttr
 | FormatArgAttr
 | UnavailableAttr
 | TransparentUnionAttr
 | BlocksAttr

(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)

 | Misc__Null__
 | Misc__Capture__
 | Misc__Cleanup__Block

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

 | IntegerLiteral
 | StringLiteral
 | FloatingLiteral
 | CharacterLiteral

 | UnaryOperator
 | BinaryOperator
 | ConditionalOperator
 | CompoundAssignOperator

 | DeclRefExpr
 | ImplicitCastExpr
 | CStyleCastExpr
 | CallExpr
 | MemberExpr
 | ArraySubscriptExpr
 | InitListExpr
 | CompoundLiteralExpr
 | ShuffleVectorExpr
 | UnaryExprOrTypeTraitExpr
 | BlockExpr
 | ParenExpr

 | ExprWithCleanups

 | VAArgExpr
 | PredefinedExpr

(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)
 | CompoundStmt
 | NullStmt
 | DeclStmt
 | IfStmt
 | ForStmt
 | WhileStmt
 | DoStmt
 | BreakStmt
 | ContinueStmt
 | SwitchStmt
 | CaseStmt
 | DefaultStmt
 | ReturnStmt
 | GotoStmt
 | LabelStmt
 | GCCAsmStmt

(* ------------------------------------------------------------------------- *)
(* Decl *)
(* ------------------------------------------------------------------------- *)

 | FunctionDecl
 | EnumDecl
 | EnumConstantDecl
 | RecordDecl
 | FieldDecl
 | IndirectFieldDecl (* ?? *)
 | Field (* ?? *)
 | TypedefDecl
 | VarDecl
 | BlockDecl
 | ParmVarDecl

 (* extern "C" { ... } *)
 | LinkageSpecDecl

 | TranslationUnitDecl
 (* with tarzan *)

(*****************************************************************************)
(* Intermediate representations *)
(*****************************************************************************)

type sexp = 
  | Paren of enum * int (* line number in .clang file *) * sexp list
  | Angle of sexp list
  (* usually for location information in the original .c file *)
  | Anchor of sexp list
  | Bracket of sexp list
  (* for types, original type and final types *)
  | Brace of Parser_clang.token list * Parser_clang.token list option
  (* everything except the TOxxx and TCxxx normally *)
  | T of Parser_clang.token 
  (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Toplevel phrases *)
(* ------------------------------------------------------------------------- *)

type program = sexp
  (* with tarzan *)
