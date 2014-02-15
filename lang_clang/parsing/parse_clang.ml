(* Yoann Padioleau
 * 
 * Copyright (C) 2013 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common 

module Flag = Flag_parsing_clang
module PI = Parse_info

open Ast_clang
open Parser_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This assumes clang-check --ast-dump does not dump the color. One can
 * do that by running it from a terminal detected as no-color (e.g. eshell),
 * or by modifying ASTDumper.cpp in clang source.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let enum__str_conv = [
 FullComment, "FullComment";
 TextComment, "TextComment";
 ParagraphComment, "ParagraphComment";
 InlineCommandComment, "InlineCommandComment";
 VerbatimLineComment, "VerbatimLineComment";
 BlockCommandComment, "BlockCommandComment";
 VisibilityAttr, "VisibilityAttr";
 DeprecatedAttr, "DeprecatedAttr";
 MaxFieldAlignmentAttr, "MaxFieldAlignmentAttr";
 AlwaysInlineAttr, "AlwaysInlineAttr";
 NoDebugAttr, "NoDebugAttr";
 ConstAttr, "ConstAttr";
 NoThrowAttr, "NoThrowAttr";
 NonNullAttr, "NonNullAttr";
 AsmLabelAttr, "AsmLabelAttr";
 PackedAttr, "PackedAttr";
 FormatAttr, "FormatAttr";
 AlignedAttr, "AlignedAttr";
 WarnUnusedResultAttr, "WarnUnusedResultAttr";
 MayAliasAttr, "MayAliasAttr";
 PureAttr, "PureAttr";
 MallocAttr, "MallocAttr";
 ReturnsTwiceAttr, "ReturnsTwiceAttr";
 UnusedAttr, "UnusedAttr";
 FormatArgAttr, "FormatArgAttr";
 UnavailableAttr, "UnavailableAttr";
 TransparentUnionAttr, "TransparentUnionAttr";
 BlocksAttr, "BlocksAttr";
 Misc__Null__, "Misc__Null__";
 Misc__Capture__, "Misc__Capture__";
 Misc__Cleanup__Block, "Misc__Cleanup__Block";
 IntegerLiteral, "IntegerLiteral";
 StringLiteral, "StringLiteral";
 FloatingLiteral, "FloatingLiteral";
 CharacterLiteral, "CharacterLiteral";
 UnaryOperator, "UnaryOperator";
 BinaryOperator, "BinaryOperator";
 ConditionalOperator, "ConditionalOperator";
 CompoundAssignOperator, "CompoundAssignOperator";
 DeclRefExpr, "DeclRefExpr";
 ImplicitCastExpr, "ImplicitCastExpr";
 CStyleCastExpr, "CStyleCastExpr";
 CallExpr, "CallExpr";
 MemberExpr, "MemberExpr";
 ArraySubscriptExpr, "ArraySubscriptExpr";
 InitListExpr, "InitListExpr";
 CompoundLiteralExpr, "CompoundLiteralExpr";
 ShuffleVectorExpr, "ShuffleVectorExpr";
 UnaryExprOrTypeTraitExpr, "UnaryExprOrTypeTraitExpr";
 BlockExpr, "BlockExpr";
 ParenExpr, "ParenExpr";
 ExprWithCleanups, "ExprWithCleanups";
 CompoundStmt, "CompoundStmt";
 NullStmt, "NullStmt";
 DeclStmt, "DeclStmt";
 IfStmt, "IfStmt";
 ForStmt, "ForStmt";
 WhileStmt, "WhileStmt";
 DoStmt, "DoStmt";
 BreakStmt, "BreakStmt";
 ContinueStmt, "ContinueStmt";
 SwitchStmt, "SwitchStmt";
 CaseStmt, "CaseStmt";
 DefaultStmt, "DefaultStmt";
 ReturnStmt, "ReturnStmt";
 GotoStmt, "GotoStmt";
 LabelStmt, "LabelStmt";
 GCCAsmStmt, "GCCAsmStmt";
 FunctionDecl, "FunctionDecl";
 EnumDecl, "EnumDecl";
 EnumConstantDecl, "EnumConstantDecl";
 RecordDecl, "RecordDecl";
 FieldDecl, "FieldDecl";
 TypedefDecl, "TypedefDecl";
 VarDecl, "VarDecl";
 BlockDecl, "BlockDecl";
 TranslationUnitDecl, "TranslationUnitDecl";

 ParmVarDecl, "ParmVarDecl";

 VAArgExpr, "VAArgExpr";
 PredefinedExpr, "PredefinedExpr";
 (* tcc *)
 IndirectFieldDecl, "IndirectFieldDecl";
 Field, "Field";
 (* spim *)
 LinkageSpecDecl , "LinkageSpecDecl";
]

let (enum_of_str, str_of_enum) =
  Common2.mk_str_func_of_assoc_conv enum__str_conv

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  Lexer_clang.line := 1;
  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    try 
      let a_token lexbuf = 
        Lexer_clang.token lexbuf
      in
      
      let rec tokens_aux acc = 
        let tok = a_token lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;

        if (match tok with Parser_clang.EOF  -> true | _ -> false)
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
      in
      tokens_aux []
  with
  | Lexer_clang.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                   (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )

let tokens_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec tokens () =
    let tok = Lexer_clang.token lexbuf in
    match tok with 
    | Parser_clang.EOF -> []
    | x -> x::tokens ()
  in
  tokens ()
    

let tokens a = 
  Common.profile_code "Parse_clang.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

type env = {
  line_open_tok: int;
  check_topar: bool;
  file: Common.filename;
}

let rec sexp_list env acc ending toks =
  let conv s = 
    try 
      enum_of_str s
    with Not_found ->
      pr2_once (spf "Could not find %s" s);
      TodoAst s
  in

  match toks with
  | x::xs when x =*= ending -> List.rev acc, xs

  (* the hex address seems actually used when one wants to crossref
   * information in the AST, e.g. implicit param references.
   *)
  | TOPar l
    ::TUpperIdent (("ImplicitCastExpr" | "CXXStaticCastExpr" 
                   | "CStyleCastExpr") as s)
    ::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l; check_topar = false} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv s, l, body)::acc) ending xs

  | TOPar l::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l; check_topar = true} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv s, l, body)::acc) ending xs


  (* ugly, clang-check -ast-dump is not that regular :( *)

  | TOPar l::TLowerIdent "super"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv ("Misc__Super__" ^ s), l, body)::acc) ending xs

  | TOPar l
    ::TLowerIdent (("public" | "private" | "protected" 
                   | "virtual") as s)
    ::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv (spf "Misc__%s__" s), l, body)::acc) ending xs

  | TOPar l::TUpperIdent "TemplateArgument"::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv "Misc__TemplateArgument__",l,body)::acc) ending xs

  | TOPar l::TLowerIdent "instance"::TCPar::xs ->
      sexp_list env (Paren (conv "Misc__Instance__", l,[])::acc) ending xs

  | TOPar l::TLowerIdent "original"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv ("Misc__Original__" ^ s), l,body)::acc) ending xs


  | TOPar l::TLowerIdent "cleanup"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv ("Misc__Cleanup__" ^ s),l, body)::acc) ending xs

  | TOPar l::TLowerIdent "capture"::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv "Misc__Capture__", l, body)::acc) ending xs


  | TOPar l::TLowerIdent (("getter" | "setter") as s1)::TUpperIdent s2
    ::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv (spf "Misc__%s__" s1 ^ s2), l, body)::acc) ending xs

  | TOPar l::TInf _::TInf _::TInf _::TUpperIdent "NULL"
    ::TSup::TSup::TSup::TCPar::xs->
      sexp_list env (Paren (conv "Misc__Null__", l, [])::acc) ending xs

  | TOPar l::TDots::TCPar::xs ->
      sexp_list env (Paren (conv "Misc__Dots__", l, [])::acc) ending xs

  | TOPar l::TLowerIdent "class"::TCPar::xs ->
      sexp_list env (Paren (conv "Misc__Class__", l, [])::acc) ending xs

  | TOPar l::TUpperIdent "ADL"::TCPar::xs ->
      sexp_list env (Paren (conv "Misc__ADL__", l, [])::acc) ending xs
  | TOPar l::TLowerIdent "no"::TUpperIdent "ADL"::TCPar::xs ->
      sexp_list env (Paren (conv "Misc__NoADL__", l, [])::acc) ending xs

  | TOPar l::TUpperIdent "CXXCtorInitializer"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv ("Misc__CXXCtorInitializer__" ^ s), l, body)::acc) ending xs

  | TOPar l::TUpperIdent "CXXCtorInitializer"::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv ("Misc__CXXCtorInitializer__"), l, body)::acc) ending xs

(*
  | TInf
    ::TUpperIdent ("UncheckedDerivedToBase"|"DerivedToBase"|"BaseToDerived")
    ::TOPar::_::TCPar::TSup::xs ->
    (* SKIP *)
    sexp_list env (Angle []::acc) ending xs
  | TInf
    ::TUpperIdent ("UncheckedDerivedToBase"|"DerivedToBase"|"BaseToDerived")
    ::TOPar::_::TArrow::_::TCPar::TSup::xs ->
    (* SKIP *)
    sexp_list env (Angle []::acc) ending xs
  | TInf
    ::TUpperIdent ("UncheckedDerivedToBase"|"DerivedToBase"|"BaseToDerived")
    ::TOPar::_::TArrow::_::TArrow::_::TCPar::TSup::xs ->
    (* SKIP *)
    sexp_list env (Angle []::acc) ending xs
*)



  | TOBracket l::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCBracket xs in
      sexp_list env (Bracket body::acc) ending xs
  | TInf l::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TSup xs in
      sexp_list env (Angle body::acc) ending xs


  | TOPar l::TUpperIdent _::xs ->
    if env.check_topar 
    then failwith (spf "%s:%d: open paren without hexint at line " env.file l)
    else
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv "Misc__SKIPPED__", l, body)::acc) ending xs
      
  | TOPar l::xs ->
    if env.check_topar 
    then failwith (spf "open paren without constructor at line %d" l)
    else
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (conv "Misc__SKIPPED__", l, body)::acc) ending xs

  | TOBrace _l::xs ->
      let (toks, _, rest) = 
        Common2.split_when (function TCBrace -> true | _ -> false) xs in
      let (toks_opt, rest) =
        (match rest with
        | TColon::TOBrace _l2::xs ->
            let (toks, _, rest) = 
              Common2.split_when (function TCBrace -> true | _ -> false) xs in
            Some toks, rest
        | _ -> None, rest
        )
      in
      sexp_list env (Brace (toks, toks_opt)::acc) ending rest
      

  | t::xs -> sexp_list env (T t::acc) ending xs
  | [] -> 
      failwith (spf "%s: unterminated sexp_list '%s' opened at line %d"
                   env.file
                   (match ending with
                   | TCPar -> "')'"
                   | TSup -> "'>'"
                   | TCBracket -> "']'"
                   | _ -> raise Impossible
                   ) env.line_open_tok)

let parse file =
  let toks = tokens file in
  let env = { line_open_tok = 0; check_topar = true; file = file } in
  let (body, _rest) = sexp_list env [] EOF toks in
  (match body with
  | [Paren (s,l, args)] -> Paren (s, l, args)
  | [Paren (s,l, args); T Error] -> 
      pr2 (spf "PB with %s" file);
      Paren (s, l, args)
  | [T Error] -> 
      pr2 (spf "PB not data at all with %s" file);
      T Error
  | [] -> 
      failwith (spf "empty file %s" file)
  | xs -> 
      pr2_gen (Common2.list_last xs);
      failwith (spf "noise after sexp, length list = %d" (List.length xs))
  )
