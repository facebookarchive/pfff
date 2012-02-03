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

open Ast_opa
open Highlight_code

module Ast = Ast_opa
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

let lexer_based_tagger = true

let is_module_name s = 
  s =~ "[A-Z].*"

type context =
  | InTop

  (* inside { }. Braces are overloaded in OPA like in C *)
  | InFunction
  | InRecord (* expr vs pattern ? *)
  | InStringInterpolation
  | InXmlInterpolation
  | InCompound
  | InCase
  (* braces and also (),  type xx = { } or type xx = ( ) when tuple *)
  | InTypedef 

  (* inside ( ) *)
  | InParameter

  | InType

  (* misc *)
  | InImport

let tag_type ~tag s ii =
  let kind = TypeMisc
  in
  tag ii kind
  
(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents,
 * to figure out what kind of ident it is (a field, a function, a type, etc).
 *)
let visit_toplevel ~tag_hook prefs  (toplevel, toks) =
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
   * is this degenerated AST, a tree of (){}[]<tag chunks.
   *)
  let toks_for_tree = toks +> Common.exclude (function
    | x when TH.is_comment x -> true
    (* todo? could try to relocate the following token to column 0? *)
    | T.Tclient _ | T.Tserver _ -> true
    | T.Tpublic _ | T.Tprivate _ -> true
    | T.Tprotected _ | T.Texposed _ -> true
    | _ -> false
  )
  in
  let tree = TV.mk_tree toks_for_tree in
  let ctx = InTop in

  (* poor's man identifier tagger.
   * todo: move this code in an ast_fuzzy_opa.ml
   *)
  let rec aux_tree ctx xs =
    match xs with
    | [] -> ()
    (* function x(...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.Paren params)
      ::(TV.Brace body)
      ::xs ->
        tag ii1 (Function (Def2 fake_no_def2));
        List.iter (aux_tree InParameter) params;
        aux_tree InFunction [(TV.Brace body)];
        aux_tree ctx xs

    (* function yy x(...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.T T.TIdent (s0, ii0))
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.Paren params)
      ::(TV.Brace body)
      ::xs ->
        aux_tree InType [(TV.T (T.TIdent (s0, ii0)))];
        tag ii1 (Function (Def2 fake_no_def2));
        List.iter (aux_tree InParameter) params;
        aux_tree InFunction [(TV.Brace body)];
        aux_tree ctx xs

    (* function yy(zz) x(...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.T T.TIdent (s0, ii0))
      ::(TV.Paren paramstype)
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.Paren params)
      ::(TV.Brace body)
      ::xs ->
        aux_tree InType [(TV.T (T.TIdent (s0, ii0)));(TV.Paren paramstype)];
        tag ii1 (Function (Def2 fake_no_def2));
        List.iter (aux_tree InParameter) params;
        aux_tree InFunction [(TV.Brace body)];
        aux_tree ctx xs

    (* database yy /x *)
    |   (TV.T T.Tdatabase _)
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.T T.TDiv _)
      ::(TV.T T.TIdent (s2, ii2))
      ::xs ->
        aux_tree InType [(TV.T (T.TIdent (s1, ii1)))];
        tag ii2 (Global (Def2 fake_no_def2));
        aux_tree ctx xs

    (* database yy(zz) /x *)
    |   (TV.T T.Tdatabase _)
      ::(TV.T T.TIdent (s0, ii0))
      ::(TV.Paren paramstype)
      ::(TV.T T.TDiv _)
      ::(TV.T T.TIdent (s2, ii2))
      ::xs ->
        aux_tree InType [(TV.T (T.TIdent (s0, ii0)));(TV.Paren paramstype)];
        tag ii2 (Global (Def2 fake_no_def2));
        aux_tree ctx xs

    (* database /xxx *)
    |   (TV.T T.Tdatabase _)
      ::(TV.T T.TDiv _)
      ::(TV.T T.TIdent (s, ii1))
      ::xs ->
        tag ii1 (Global (Def2 fake_no_def2));
        aux_tree ctx xs

    (* type x = { ... } *)
    |   (TV.T T.Ttype _)
      ::(TV.T (T.TIdent (s, ii1)))
      ::(TV.T (T.TEq ii2))
      ::TV.Brace bodytype
      ::xs ->
        tag ii1 (TypeDef Def);
        List.iter (aux_tree InTypedef) bodytype;
        aux_tree ctx xs

    (* todo: type x(yy) = *)

    (* todo? package ... *)
    (* todo? module x = {...} *)

    (* todo? x = ... at toplevel *)


    (* INSIDE Typedef *)
    
    (* yy x *)
    |  (TV.T T.TIdent (s1, ii1))
     ::(TV.T T.TIdent (s2, ii2))
     ::xs when ctx = InTypedef ->
       tag_type ~tag s1 ii1;
       tag ii2 (Field (Def2 fake_no_def2));
       aux_tree ctx xs

    (* INSIDE Type *)
    | TV.T (T.TIdent (s1, ii1))::xs when ctx = InType ->
        tag_type ~tag s1 ii1;
        aux_tree ctx xs

    (* REST *)
    | x::xs -> 
        (match x with
        | TV.T _ -> ()
        | TV.Paren xxs ->
            List.iter (aux_tree ctx) xxs
        | TV.Brace xxs ->
            List.iter (aux_tree ctx) xxs
        | TV.Bracket xxs ->
            List.iter (aux_tree ctx) xxs
        | TV.Xml (xxs, yys) ->
            raise Todo
        );
        aux_tree ctx xs
  in
  aux_tree ctx tree;

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)
  let rec aux_toks xs = 
    match xs with
    | [] -> ()
    (* a little bit pad specific *)
    |   T.TComment(ii)
      ::T.TCommentNewline (ii2)
      ::T.TComment(ii3)
      ::T.TCommentNewline (ii4)
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
    | T.Tpackage ii1::T.TIdent (s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Module Def);
        aux_toks xs

    | T.Tmodule ii1::T.TIdent (s, ii2)::xs ->
        tag ii2 (Module Def);
        aux_toks xs


    | (T.Ttype ii1 | T.Tand ii1)::T.TIdent (s, ii2)::xs ->
        tag ii2 (TypeDef Def);
        aux_toks xs

     (* can't put (Function Def) here because the function keyword
      * can also be followed by the return type.
      *)
    | T.TIdent (s, ii1)::T.TEq _::T.Tparser _::xs ->
        tag ii1 (Function (Def2 fake_no_def2));
        aux_toks xs

    | T.T_XML_ATTR("id", _)::T.TEq(_)::T.TSharpIdent(s, ii)::xs ->
        tag ii (Global (Def2 fake_no_def2));
        aux_toks xs

    | T.T_XML_ATTR("id", _)::T.TEq(_)
      ::T.TGUIL(ii)::T.T_ENCAPSED(_, ii1)::xs ->
        (* this is for style, not for modification *)
        aux_toks xs

    | T.T_XML_ATTR(("class"|"style"), _)::T.TEq(_)
        ::T.TGUIL(ii)::T.T_ENCAPSED(_, ii1)::xs ->
        tag ii1 EmbededStyle;
        aux_toks xs

    | T.T_XML_ATTR(("href"|"src"|"xmlns"), _)::T.TEq(_)
        ::T.TGUIL(ii)::T.T_ENCAPSED(_, ii1)::xs ->
        tag ii1 EmbededUrl;
        aux_toks xs

    (* uses *)
    | T.TIdent(s, ii1)::T.TColon ii2::xs ->
        tag ii1 (Field (Use2 fake_no_use2));
        aux_toks xs

    | T.TTilde(ii1)::T.TIdent (_,ii2)::xs ->
        tag ii2 (Field (Use2 fake_no_use2));
        aux_toks xs

    |   T.TIdent (s1, ii1)::T.TDot ii2
      ::T.TIdent (s3, ii3)::T.TOParen(ii4)::xs ->
        if is_module_name s1 
        then tag ii1 (Module (Use));
        tag ii3 (Function (Use2 fake_no_use2));
        
        aux_toks xs

    |   T.TIdent (s1, ii1)::T.TDot ii2::T.TIdent (s3, ii3)::xs ->
        if is_module_name s1 
        then tag ii1 (Module (Use));
        (* too many FPs: tag ii3 (Field (Use2 fake_no_use2)); *)
        aux_toks xs

    | T.TSharp _::T.TIdent(s1, ii1)::xs ->
        tag ii1 (Global (Use2 fake_no_use2));
        aux_toks xs

    | x::T.TDiv _::T.TIdent(s1, ii1)::xs ->
        if not (Hashtbl.mem already_tagged ii1)
        then tag ii1 (Global (Use2 fake_no_use2));
        aux_toks xs

    | x::xs ->
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

    | T.TCommentSpace ii | T.TCommentNewline ii | T.TCommentMisc ii -> ()

    | T.TUnknown ii -> tag ii Error
    | T.EOF ii-> ()

    (* values  *)

    | T.TIdent(("true" | "false"), ii) -> 
        tag ii Boolean
    | T.TInt (s,ii) | T.TFloat (s,ii) ->
        tag ii Number
    | T.TGUIL ii | T.T_ENCAPSED (_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii String

    (* keyword types  *)
    | T.TIdent(("int" | "float"), ii) -> tag ii TypeInt
    | T.TIdent("bool", ii) -> tag ii TypeMisc
    | T.TIdent("string", ii) -> tag ii TypeMisc
    | T.TIdent(("list" | "option" | "intmap" | "stringmap"), ii) -> tag ii TypeMisc
    | T.TIdent("void", ii) -> tag ii TypeVoid

    | T.Tint ii | T.Tfloat ii -> tag ii TypeInt
    | T.Tstring ii -> tag ii TypeMisc

    | T.TTypeVar(_, ii) -> tag ii TypeMisc

    (* keywords *)

    | T.Tif ii | T.Tthen ii | T.Telse ii -> 
        tag ii KeywordConditional
    | T.Tmatch ii | T.Tcase ii | T.Tdefault ii
        -> tag ii KeywordConditional

    | T.Ttype ii
    | T.Twith ii
    | T.Tas ii
        -> tag ii Keyword

    | T.Tpackage ii | T.Tmodule ii | T.Timport ii
        -> tag ii KeywordModule

    | T.Tdo ii -> tag ii KeywordLoop


    | T.Tprotected ii | T.Texposed ii
    | T.Tclient ii    | T.Tserver ii
    | T.Tpublic ii | T.Tprivate ii
          -> tag ii Keyword

    | T.Tforall ii
    | T.Texternal ii
    | T.Tparser ii
    | T.Tdatabase ii
    | T.Tcss ii
    | T.Tend ii
    | T.Tbegin ii
    | T.Trec ii
    | T.Tand ii
    | T.Tval ii
    | T.Tor ii
    | T.Tfunction ii
        -> tag ii Keyword

    (* xml  *)
    | T.T_XML_OPEN_TAG (s, ii) ->
        (* todo: match s with ...
         * look the html highlighter in lang_html/ ?
         *)
        tag ii Keyword
    | T.T_XML_CLOSE_TAG (sopt, ii) ->
        (* todo: match s with ... *)
        tag ii Keyword

    | T.T_XML_ATTR (s, ii) ->
        (* todo: match s with ...
         * look the html highlighter in lang_html/ ?
         *)
        tag ii Keyword
    | T.T_XML_MORE ii ->
        tag ii Keyword
    | T.T_XML_SLASH_GT ii ->
        tag ii Keyword
    | T.T_XML_TEXT (s, ii) -> tag ii String

    (* css *)
    | T.TSharpIdent (s, ii) ->
        ()

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

    | T.TAnd ii
        -> tag ii Operator

    | T.TNotEq ii
    | T.TEqEq ii

    | T.TXor ii
    | T.TOr ii
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

    | T.TExternalIdent (s, ii) ->
        ()
    | T.TIdent (s, ii) ->
        ()
  );
  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  
  (* -------------------------------------------------------------------- *)

  ()
