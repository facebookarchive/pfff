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

open Ast_fuzzy_opa
open Entity_code open Highlight_code

module Ast = Ast_fuzzy_opa
module HC = Highlight_code
module T = Parser_opa
module TH = Token_helpers_opa
module TV = Token_views_opa
module V = Visitor_opa

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Syntax highlighting for OPA code for codemap.
 * 
 * todo: this code should actually be abused to generate the light
 * database and the TAGS file (because codemap needs/detects def and
 * use of entities).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in codemap when a light
 * database is passed via -with_info to codemap.
 *)
let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

let is_module_name s =
  s =~ "[A-Z].*"

type in_context =
  | InTop

  (* inside { }. Braces are overloaded in OPA like in C *)
  | InFunction
  (* braces and also (),  type xx = { } or type xx = ( ) when tuple *)
  (* InTypedef, see type_def  *)

(*
  | InRecord (* expr vs pattern ? *)
  | InStringInterpolation
  | InXmlInterpolation
  | InCompound
  | InCase

  (* inside ( ) *)
  | InParameter

  | InType

  (* misc *)
  | InImport
  | InPackage
*)

type context = {
  ctx: in_context;

  params: string list;
  locals: string list;
  globals: string list;
  functions: string list;
}
let default_ctx = {
  ctx = InTop;
  params = [];
  locals = [];
  globals = [];
  functions = [];
}
 
(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents,
 * to figure out what kind of ident it is (a field, a function, a type, etc).
 *)
let visit_toplevel ~tag_hook _prefs  (_toplevel, toks) =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in
  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *) 
  (* -------------------------------------------------------------------- *)
  (* parsing OPA turns out to be difficult so for now the best we have
   * is this fuzzy AST.
   *)
  let toks_for_fuzzy_ast = Ast_fuzzy_opa.toks_for_ast_fuzzy toks in
  let xs = Token_views_opa.mk_tree toks_for_fuzzy_ast in
  let xs = Ast_fuzzy_opa.mk_tree xs in

  (* poor's man identifier tagger *)
  let rec tree ctx = function
    | Ast.Function def ->
        def.f_name +> Common.do_option (fun name ->
          let info = info_of_name name in
          tag info (HC.Entity (Function, (Def2 fake_no_def2)));
        );
        Common.do_option (type_ ctx) def.f_ret_type;
        List.iter (parameter ctx) def.f_params;
        body { ctx with 
          params = def.f_params +> Common.map_filter (function
          | Param (_, name) -> Some (str_of_name name)
          | ParamOther _ -> None
          );
          ctx = InFunction;
        } def.f_body;
        ()

    | Ast.TypeDef (name, tdef) ->
        let info = info_of_name name in
        tag info (Entity (Type, Def2 fake_no_def2));
        type_def ctx tdef

    | Ast.Module (name, xs) ->
        let info = info_of_name name in
        tag info (Entity (Module, (Def2 fake_no_def2)));
        tree_list ctx xs

    | Ast.VarDef (typ, name) ->
        let info = info_of_name name in
        Common.do_option (type_ ctx) typ;

        if ctx.ctx =*= InTop
        then tag info (Entity (Global, (Def2 fake_no_def2)))
        else tag info (Local Def)

    | TreeTodo -> ()
    | T _ -> ()
    | Paren xxs ->
        xxs +> List.iter (tree_list ctx)
    | Brace xxs ->
        xxs +> List.iter (tree_list ctx)
    | Bracket xxs ->
        xxs +> List.iter (tree_list ctx)
    | Xml ((_, _)) ->
        raise Todo
  and type_ ctx = function
    | TyName (_qu, name) ->
        let info = info_of_name name in
        (* todo: different color for int/bool/list/void etc? *)
        tag info (Entity (Type, (Use2 fake_no_use2)))
        
    | TyVar _name -> ()
    | TyApp (_long_name, xs) ->
        List.iter (type_ ctx) xs
    | TyOther xs -> tree_list ctx xs
  and type_def ctx = function
    | TyRecord xs ->
        List.iter (field ctx) xs
    | TypeDefOther xs ->
        tree_list ctx xs

  and field ctx = function
    | Ast.Field (typ, name) ->
        let info = info_of_name name in
        tag info (Entity (Field, (Def2 fake_no_def2)));
        Common.do_option (type_ ctx) typ
    | FieldOther xs -> 
        tree_list ctx xs

  and parameter ctx = function
    | Param (typ, name) ->
        let info = info_of_name name in
        tag info (Parameter Def);
        Common.do_option (type_ ctx) typ

    | ParamOther xs ->
        tree_list ctx xs
  and body ctx xs = 
    tree_list ctx xs

(*
*)

  and tree_list ctx xs =
    match xs with
    | [] -> ()

    | (T (T.TIdent (_s, info)))::(Paren parens)::xs ->
        tag info (Entity (Function, (Use2 fake_no_use2)));
        tree_list ctx ((Paren parens)::xs)

    | (T (T.TIdent (s, info)))::xs ->
        (match () with
        | _ when List.mem s (ctx.params) ->
            tag info (Parameter Use)
        | _ when List.mem s (ctx.locals) ->
            tag info (Local Use)
        | _ when List.mem s (ctx.globals) ->
            tag info (Entity (Global, (Use2 fake_no_use2)))
(*
        | _ when List.mem s (ctx.functions) ->
            tag info (PointerCall)
*)
        | _ -> ()
        );
        tree_list ctx xs

    | (Ast.Function (def) as x)::xs ->
        tree ctx x;
        let ctx =
          match def.f_name with
          | None -> ctx
          | Some name ->
              let s = str_of_name name in
              { ctx with functions = s::ctx.functions; }
        in
        tree_list ctx xs
        


    | (Ast.VarDef (_typ, name) as x)::xs ->
        let s = str_of_name name in
        tree ctx x;
        let ctx = 
          if ctx.ctx =*= InTop
          then { ctx with globals = s::ctx.globals; }
          else { ctx with locals = s::ctx.locals; }
        in
        tree_list ctx xs

(*
    |  (TV.T (T.TIdent (s1, ii1)))
     ::(TV.T (T.TEq _))
     ::(TV.T (T.TExternalIdent (s2, ii2)))
       tag ii1 (Function (Def2 fake_no_def2));
       tag ii2 CppOther;
*)
        
    | x::xs ->
        tree ctx x;
        tree_list ctx xs
  in
  tree_list (default_ctx) xs;

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)
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

    (* When there is a parse error, the AST will not contain anything,
     * but we can still try to tag certain things. We can infer if an
     * ident is a func, or type, or module based on the few tokens around.
     * 
     * This may look ridiculous to do such semantic tagging using tokens 
     * instead of the full AST but many OPA files could not parse with
     * the default parser so having a solid token-based tagger
     * is still useful as a last resort. 
     * 
     * This is quite close to what you would do with emacs font-lock-mode.
     *)

    (* another poor's man identifier tagger *)

    (* defs *)

    (* FPs, can be the ident after that
    | T.Tpackage ii1::T.TIdent (s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Module Def);
        aux_toks xs
    *)

    | (T.Ttype _ii1 | T.Tand _ii1)::T.TIdent (_s, ii2)::xs ->
        tag ii2 (Entity (Type, Def2 fake_no_def2));
        aux_toks xs

     (* can't put (Function Def) here because the function keyword
      * can also be followed by the return type.
      *)
    | T.TIdent (_s, ii1)::T.TEq _::T.Tparser _::xs ->
        tag ii1 (Entity (Function, (Def2 fake_no_def2)));
        aux_toks xs

    | T.T_XML_ATTR("id", _)::T.TEq(_)::T.TSharpIdent(_s, ii)::xs ->
        tag ii (Entity (Global, (Def2 fake_no_def2)));
        aux_toks xs

    | T.T_XML_ATTR("id", _)::T.TEq(_)
      ::T.TGUIL(_ii)::T.T_ENCAPSED(_, _ii1)::xs ->
        (* this is for style, not for modification *)
        aux_toks xs

    | T.T_XML_ATTR(("class"|"style"), _)::T.TEq(_)
        ::T.TGUIL(_ii)::T.T_ENCAPSED(_, ii1)::xs ->
        tag ii1 EmbededStyle;
        aux_toks xs

    | T.T_XML_ATTR(("href"|"src"|"xmlns"), _)::T.TEq(_)
        ::T.TGUIL _::T.T_ENCAPSED(_, ii1)::xs ->
        tag ii1 EmbededUrl;
        aux_toks xs

    | T.T_XML_ATTR(s, ii)::T.TEq(_)::xs when s =~ "on.*" ->
        tag ii (Entity (Method, (Def2 fake_no_def2)));
        aux_toks xs

    (* uses *)
    | T.TIdent(_s, ii1)::T.TColon _::xs ->
        tag ii1 (Entity (Field, (Use2 fake_no_use2)));
        aux_toks xs

    | T.TTilde _::T.TIdent (_,ii2)::xs ->
        tag ii2 (Entity (Field, (Use2 fake_no_use2)));
        aux_toks xs

    |   T.TIdent (s1, ii1)::T.TDot _::xs ->
        if is_module_name s1 
        then tag ii1 (Entity (Module, (Use2 fake_no_use2)));
        aux_toks xs

    | T.TSharp _::T.TIdent(_s1, ii1)::xs ->
        tag ii1 (Entity (Global, (Use2 fake_no_use2)));
        aux_toks xs

    | _x::T.TDiv _::T.TIdent(_s1, ii1)::xs ->
        if not (Hashtbl.mem already_tagged ii1)
        then tag ii1 (Entity (Global, (Use2 fake_no_use2)));
        aux_toks xs

    | T.TAt _::T.TIdent(_s1, ii1)::xs ->
        tag ii1 CppOther;
        aux_toks xs

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
  (* -------------------------------------------------------------------- *)
  toks +> List.iter (fun tok -> 
    match tok with

    (* comments *)

    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii) (* could be Estet tagged *)
        then tag ii Comment

    | T.TCommentSpace _ii | T.TCommentNewline _ii | T.TCommentMisc _ii -> ()

    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii -> ()

    (* values  *)

    | T.TIdent(("true" | "false"), ii) -> 
        tag ii Boolean
    | T.TIdent(("none"), ii) -> 
        tag ii Null

    | T.TInt (_s,ii) | T.TFloat (_s,ii) ->
        tag ii Number
    | T.TGUIL ii | T.T_ENCAPSED (_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii String

    | T.T_CSS_TEXT ii ->
        tag ii EmbededStyle
    | T.T_PARSER_BEFORE_ARROW ii ->
        tag ii Verbatim


    (* keyword types  *)
    | T.TIdent(("int" | "float"), ii) -> tag ii TypeInt
    | T.TIdent("bool", ii) -> tag ii TypeInt
(* FPs because sometimes used as a function name, as in Random.string 
    | T.TIdent("string", ii) -> tag ii TypeMisc
*)
    | T.TIdent(("list" | "option" | "either" | "intmap" | "stringmap"), ii) -> 
        tag ii TypeInt
    | T.TIdent("void", ii) -> tag ii TypeVoid

    | T.Tint ii | T.Tfloat ii -> tag ii TypeInt
    | T.Tstring ii -> tag ii TypeInt

    | T.TTypeVar(_, ii) -> tag ii TypeInt

    (* keywords *)

    | T.Tif ii | T.Tthen ii | T.Telse ii -> 
        tag ii KeywordConditional
    | T.Tmatch ii | T.Tcase ii | T.Tdefault ii
        -> tag ii KeywordConditional

    | T.Ttype ii
    | T.Tor ii
        -> tag ii Keyword
    | T.Twith ii
    | T.Tas ii
        -> tag ii Keyword

    | T.Tpackage ii | T.Timport ii
    | T.Tmodule ii 
        -> tag ii KeywordModule

    | T.Tdo ii -> tag ii KeywordLoop

    | T.Tprotected ii | T.Texposed ii
    | T.Tclient ii    | T.Tserver ii
    | T.Tpublic ii | T.Tprivate ii
          -> tag ii Keyword

    | T.Tparser ii
    | T.Tdatabase ii
    | T.Tcss ii

    | T.Tval ii | T.Texternal ii
    | T.Tforall ii

    | T.Tbegin ii | T.Tend ii
    | T.Trec ii | T.Tand ii
    | T.Tfunction ii
        -> tag ii Keyword

    (* xml  *)
    | T.T_XML_OPEN_TAG (_s, ii) ->
        (* todo: match s with ...? look html highlighter in lang_html/ ? *)
        tag ii EmbededHtml

    | T.T_XML_CLOSE_TAG (_sopt, ii) ->
        (* todo: match s with ... *)
        tag ii EmbededHtml

    | T.T_XML_ATTR (_s, ii) ->
        (* todo: match s with ...? look html highlighter in lang_html/ ? *)
        if not (Hashtbl.mem already_tagged ii)
        then tag ii EmbededHtmlAttr

    | T.T_XML_MORE ii ->
        tag ii EmbededHtml
    | T.T_XML_SLASH_GT ii ->
        tag ii EmbededHtml
    | T.T_XML_TEXT (_s, ii) -> tag ii String

    (* css *)
    | T.TSharpIdent (_s, _ii) ->
        ()

    (* ?? *)
    | T.TSharp ii
        -> tag ii Punctuation

    (* symbols *)
    | T.TEq ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Punctuation

    | T.TOp(_, ii) -> tag ii Operator

    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii
          -> tag ii Punctuation

    | T.TPlus ii | T.TMinus ii
    | T.TLess ii | T.TMore ii
        -> tag ii Operator

    | T.TDot (ii)
    | T.TColon (ii)
        -> tag ii Punctuation

    | T.TStar ii
      -> tag ii Operator

    | T.TEqEq ii | T.TNotEq ii
        -> tag ii Operator

    | T.TAnd ii | T.TOr ii | T.TXor ii 
        -> tag ii Operator

    | T.TComma ii
        -> tag ii Punctuation

    | T.TDiv ii
        -> tag ii Operator

    | T.TArrow ii
    | T.TQuestion ii
    | T.TSemiColon ii
        -> tag ii Punctuation

    | T.THat ii
    | T.TOrOr ii
    | T.TAndAnd ii
    | T.TAntiSlash ii
    | T.TAt ii
    | T.TUnderscore ii
    | T.TTilde ii
        -> tag ii Punctuation

    (* rest *)

    | T.TExternalIdent (_s, _ii) ->
        ()
    | T.TIdent (_s, _ii) ->
        ()
  );
  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  
  (* -------------------------------------------------------------------- *)

  ()
