(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012 Facebook
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

open Ast_java
open Entity_code open Highlight_code
module Ast = Ast_java
module V = Visitor_java
module T = Parser_java
module TH = Token_helpers_java
module HC = Highlight_code

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

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
 *)

let visit_toplevel ~tag_hook _prefs (ast, toks) =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in
  let tag_ident (id: Ast_java.ident) categ = 
    let (_s, ii) = id in
    tag ii categ
  in

  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *) 
  (* tagging the idents of the AST *)
  let visitor = V.mk_visitor { V.default_visitor with

    (* defs *)
    V.kdecl = (fun (k, _) d ->
      (match d with
      | Ast.Class x ->
          let ident = x.cl_name in
          tag_ident ident (Entity (Class, (Def2 fake_no_def2)))
      | Ast.Field x ->
          let var = x.f_var in
          let ident = var.v_name in
          tag_ident ident (Entity (Field, (Def2 fake_no_def2)))
      | Ast.Method x ->
          let var = x.m_var in
          let ident = var.v_name in
          tag_ident ident (Entity (Method, (Def2 fake_no_def2)));
          x.m_formals +> List.iter (fun v ->
            let ident = v.v_name in
            tag_ident ident (Parameter Def)
          )
      | Ast.Enum x ->
          let ident = x.en_name in
          tag_ident ident (Entity (Class, (Def2 fake_no_def2)))
      | Ast.Init (_bool, _stmt) ->
          ()
      );
      k d
    );
    V.kstmt = (fun (k, _) x ->
      (match x with
      | LocalVar v ->
        let ident = v.f_var.v_name in
        tag_ident ident (Local Def)
      | _ -> ()
      );
      k x
    );

    (* uses *)
    V.kexpr = (fun (k, _) e ->
      (match e with
      | Call (Dot (e, ident), args) ->
          tag_ident ident (HC.Entity (Method, (Use2 fake_no_use2)));
          k e;
          List.iter k args
        
      | Dot (e, ident) ->
          tag_ident ident (Entity (Field, (Use2 fake_no_use2)));
          k e
      | _ -> k e
      );
    );

    V.ktype = (fun (k, _) e ->
      (match e with
      (* done on PRIMITIVE_TYPE below *)
      | TBasic (_s, _ii) -> ()
      | TClass xs -> 
          (match List.rev xs with
          | [] -> raise Impossible
          | (id, _targs)::xs -> 
            tag_ident id (Entity (Type, (Use2 fake_no_use2)));
            xs +> List.iter (fun (id, _targs) ->
              tag_ident id (Entity (Module, (Use2 fake_no_use2)))
            )
          )
      | TArray _ -> ()
      );
      k e
    );
  }
  in
  visitor (AProgram ast);

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  let rec aux_toks xs = 
    match xs with
    | [] -> ()
    (* a little bit pad specific *)
    |   T.TComment(ii)
      ::T.TCommentNewline (_ii2)
      ::T.TComment(ii3)
      ::T.TCommentNewline (_ii4)
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

    (* less: poor's man identifier tagger? *)
    (* defs *)
    (* uses *)

    | _x::xs ->
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
        then tag ii Comment
    | T.TCommentSpace ii ->
        if not (Hashtbl.mem already_tagged ii)
        then ()
        else ()

    | T.TCommentNewline _ii -> ()

    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii -> ()

    (* values  *)

    | T.TString (_s,ii) ->
        tag ii String
    | T.TChar (_s, ii) ->
        tag ii String
    | T.TFloat (_s,ii) | T.TInt (_s,ii) ->
        tag ii Number

    | T.LITERAL (s, ii) ->
        (match s with
        | "true" | "false" -> tag ii Boolean
        | "null" -> tag ii Null
        | _ -> ()
        )

    | T.PRIMITIVE_TYPE (s, ii) ->
        (match s with
        | "void" -> tag ii TypeVoid
        | "boolean" -> tag ii TypeInt
        | _ -> tag ii TypeInt
        )

    | T.OPERATOR_EQ (_s, ii) ->
        tag ii Operator

    | T.IDENTIFIER (_s, _ii) ->
        ()

    (* keywords  *)
    | T.BOOLEAN ii -> tag ii TypeInt

    | T.BYTE ii | T.CHAR ii | T.INT ii | T.SHORT ii | T.LONG ii
          -> tag ii TypeInt

    | T.DOUBLE ii | T.FLOAT ii
          -> tag ii TypeInt

    | T.VOID ii -> tag ii TypeVoid
       
    | T.CLASS ii  | T.ABSTRACT ii | T.INTERFACE ii
    | T.PRIVATE ii | T.PROTECTED ii | T.PUBLIC ii
    | T.THIS ii | T.SUPER ii | T.NEW ii 
    | T.INSTANCEOF ii
    | T.EXTENDS ii  | T.FINAL ii | T.IMPLEMENTS ii
          -> tag ii KeywordObject

    | T.BREAK ii | T.CONTINUE ii
    | T.RETURN ii | T.GOTO ii
          -> tag ii Keyword

    | T.TRY ii  | T.THROW ii | T.THROWS ii
    | T.CATCH ii  | T.FINALLY ii
          -> tag ii KeywordExn

    | T.IF ii | T.ELSE ii 
          -> tag ii KeywordConditional

    | T.FOR ii | T.DO ii | T.WHILE ii
          -> tag ii KeywordLoop

    | T.SWITCH ii
    | T.CASE ii
    | T.DEFAULT ii
        -> tag ii KeywordConditional

    | T.PACKAGE ii
    | T.IMPORT ii
        -> tag ii KeywordModule

    | T.NATIVE ii
        -> tag ii Keyword

    | T.VOLATILE ii | T.STATIC ii
    | T.CONST ii
        -> tag ii Keyword

    | T.SYNCHRONIZED ii
        -> tag ii Keyword

    | T.STRICTFP ii
    | T.TRANSIENT ii
    | T.ASSERT ii
        -> tag ii Keyword

    (* java ext *)
    | T.ENUM ii
        -> tag ii Keyword

    | T.AT ii ->
        tag ii Punctuation

    | T.DOTS ii ->
        tag ii Punctuation

    (* symbols *)

    | T.LP ii | T.RP ii
    | T.LC ii | T.RC ii
    | T.LB ii  | T.RB ii

    | T.LB_RB ii

    | T.SM ii
    | T.CM ii
    | T.DOT ii

    | T.EQ ii  

    | T.LT ii | T.LT2 ii
    | T.GT ii 

    | T.NOT ii  | T.COMPL ii

    | T.COND ii
    | T.COLON ii
    | T.EQ_EQ ii

    | T.LE ii  | T.GE ii
    | T.NOT_EQ ii
    | T.AND ii  | T.OR ii
    | T.INCR ii | T.DECR ii
    | T.PLUS ii  | T.MINUS ii  | T.TIMES ii  | T.DIV ii
    | T.AND_AND ii | T.OR_OR ii | T.XOR ii

    | T.MOD ii
    | T.LS ii
    | T.SRS ii
    | T.URS ii
        -> tag ii Punctuation
  );
  ()
