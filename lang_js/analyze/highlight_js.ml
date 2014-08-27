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

open Entity_code open Highlight_code
module Ast = Ast_js
module T = Parser_js
module TH = Token_helpers_js
module E = Entity_code
module HC = Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_program ~tag_hook _prefs (*db_opt *) (ast, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in

  (* many class idions are recognized in Class_js *)
  let hcomplete_name_of_info =
    Class_js.extract_complete_name_of_info ast
  in


  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  let toks' = toks +> Common.exclude (function
    | T.TCommentSpace _ -> true
    | T.TCommentNewline _ -> true
    | T.TComment _ -> true
    | _ -> false
  )
  in

  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* the class extraction stuff as JX.install('ClassFoo'... )
     * is not handled in class_js.ml
     *)

    (* todo: move pattern in Class_js ? *)
    | T.T_IDENTIFIER ("JX", info)
      ::T.T_PERIOD(_)
      ::T.T_IDENTIFIER ("Stratcom", _)
      ::T.T_PERIOD(_)
      ::T.T_IDENTIFIER ("listen", _)
      ::T.T_LPAREN(_)
      ::xs ->
        let rec find_first_string xs =
          match xs with
          | [] ->
              pr2 (spf "PB: find_first_string: at %s"
                      (Parse_info.string_of_info info)
              );
              raise Not_found
          | T.T_STRING (_, ii)::_ -> ii
          | _x::xs -> find_first_string xs
        in
        (try
            let ii_string = find_first_string xs in
            tag ii_string (HC.Entity (Method, (Def2 fake_no_def2)));
          with Not_found -> ()
        );
        aux_toks xs

    | T.T_IDENTIFIER (_, ii2)
      ::T.T_COLON(_ii3)
      ::T.T_FUNCTION _
      ::xs
       (* todo? when Parse_info.col_of_info ii1 = 0 *)
      ->
        tag ii2 (Entity (Method, (Def2 NoUse)));
        aux_toks xs

    | T.T_FUNCTION(_ii1)
      ::T.T_IDENTIFIER (_, ii2)
      ::T.T_LPAREN(_)
      ::xs
       (* when Parse_info.col_of_info ii1 = 0 ? *)
      ->
        tag ii2 (Entity (Function, (Def2 NoUse)));
        aux_toks xs


    | T.T_IDENTIFIER ("JX", ii1)
      ::T.T_PERIOD(_)
      ::T.T_IDENTIFIER (_, ii_last)
      ::T.T_LPAREN(_)
      ::xs when Parse_info.col_of_info ii1 = 0 ->

        tag ii_last (Entity (Global, (Def2 NoUse)));
        aux_toks xs

    | T.T_IDENTIFIER ("JX", ii1)
      ::T.T_PERIOD(_)
      ::T.T_IDENTIFIER (_, ii_last)
      ::T.T_LPAREN(_)
      ::xs when Parse_info.col_of_info ii1 <> 0 ->

        tag ii_last (Entity (Class, (Use2 fake_no_use2)));
        aux_toks xs

    | T.T_IDENTIFIER ("JX", ii1)
      ::T.T_PERIOD(_)
      ::T.T_IDENTIFIER (_, _ii2)
      ::T.T_PERIOD(_)
      ::T.T_IDENTIFIER (_, ii_last)
      ::T.T_LPAREN(_)
      ::xs when Parse_info.col_of_info ii1 = 0 ->

        tag ii_last (Entity (Global, (Def2 NoUse)));
        aux_toks xs

    | _x::xs ->
        aux_toks xs
  in
  aux_toks toks';


  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *)
  (* TODO!! *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  toks +> List.iter (fun tok ->
    match tok with
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Comment
    | T.TCommentSpace (_ii) | T.TCommentNewline (_ii) -> ()

    | T.T_NULL (ii) -> tag ii Null
    | T.T_FALSE (ii) | T.T_TRUE (ii) -> tag ii Boolean
    | T.T_NUMBER (_, ii) -> tag ii Number

    (* Both strings and regular identifiers can be used to represent
     * entities such as classes so have to look for both
     *)
    | T.T_STRING (_, ii) | T.T_ENCAPSED_STRING(_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then
          let kind_name_opt =
            Common2.hfind_option ii hcomplete_name_of_info
          in
          (match kind_name_opt with
          | Some (E.Class, _fullname) ->

              (* rewrite ii to remove the enclosing "" ?
               * but the Parsing2 then use the full info as
               * the key of the hash and so may not find
               * back the category for a token.
               *)
              (*let ii' = Parse_info.rewrap_str s ii in*)
              tag ii (Entity (Class, (Def2 fake_no_def2)));
          | Some (E.Method, _) ->
              (* jsspec use strings for method names *)
              ()

          | Some (_) ->
              failwith ("WEIRD: a string is not a class at " ^
                           Parse_info.string_of_info ii)
          | None ->
              tag ii String
          )
    | T.T_BACKQUOTE ii -> tag ii String
    | T.T_DOLLARCURLY ii -> tag ii String

    | T.T_REGEX (_, ii) -> tag ii String

    | T.T_IDENTIFIER (_, ii) ->
       if not (Hashtbl.mem already_tagged ii)
       then
        let kind_name_opt =
          Common2.hfind_option ii hcomplete_name_of_info
        in
        (match kind_name_opt with
        | Some (E.Class, _fullname) ->
            tag ii (Entity (Class, (Def2 fake_no_def2)))
        | Some (E.Method, _fullname) ->
            tag ii (Entity (Method, (Def2 fake_no_def2)))
(* less:
        | Some (E.Method E.StaticMethod, _fullname) ->
            tag ii (StaticMethod (Def2 fake_no_def2))
*)
        | Some (_) ->
            failwith ("WEIRD: a identfier is not a class or method at " ^
                           Parse_info.string_of_info ii)
        | None ->
            ()
        )


    | T.T_FUNCTION (ii) ->  tag ii Keyword

    | T.T_IF (ii)  | T.T_SWITCH (ii) | T.T_ELSE (ii) ->
        tag ii KeywordConditional

    | T.T_IN (ii) | T.T_INSTANCEOF (ii) | T.T_RETURN (ii) | T.T_THIS (ii) ->
         tag ii Keyword


    | T.T_THROW (ii) | T.T_TRY (ii) | T.T_CATCH (ii) | T.T_FINALLY (ii) ->
        tag ii KeywordExn

    | T.T_CLASS ii | T.T_EXTENDS ii  -> tag ii KeywordObject

    | T.T_INTERFACE ii -> tag ii KeywordObject

    | T.T_XHP_TEXT (_, ii) -> tag ii String
    | T.T_XHP_ATTR (_, ii) -> tag ii (Entity (Field, (Use2 fake_no_use2)))

    | T.T_XHP_CLOSE_TAG (_, ii) -> tag ii EmbededHtml
    | T.T_XHP_SLASH_GT ii -> tag ii EmbededHtml
    | T.T_XHP_GT ii -> tag ii EmbededHtml
    | T.T_XHP_OPEN_TAG (_, ii) -> tag ii EmbededHtml

    | T.T_STATIC ii -> tag ii Keyword

    | T.T_WHILE (ii) | T.T_DO (ii) | T.T_FOR (ii) -> tag ii KeywordLoop

    | T.T_VAR (ii) ->
        tag ii Keyword

    | T.T_WITH (ii) ->
        tag ii Keyword

    | T.T_CONST (ii) ->
        tag ii Keyword
    | T.T_BREAK (ii)
    | T.T_CASE (ii)
    | T.T_CONTINUE (ii)
    | T.T_DEFAULT (ii)
    | T.T_NEW (ii)

    | T.T_LCURLY (ii)
    | T.T_RCURLY (ii)
    | T.T_LPAREN (ii)
    | T.T_RPAREN (ii)
    | T.T_LBRACKET (ii)
    | T.T_RBRACKET (ii)
    | T.T_SEMICOLON (ii)
    | T.T_COMMA (ii)
    | T.T_PERIOD (ii)
    | T.T_DOTS ii
        -> tag ii Punctuation

    | T.T_RSHIFT3_ASSIGN (ii)
    | T.T_RSHIFT_ASSIGN (ii)
    | T.T_LSHIFT_ASSIGN (ii)
    | T.T_BIT_XOR_ASSIGN (ii)
    | T.T_BIT_OR_ASSIGN (ii)
    | T.T_BIT_AND_ASSIGN (ii)
    | T.T_MOD_ASSIGN (ii)
    | T.T_DIV_ASSIGN (ii)
    | T.T_MULT_ASSIGN (ii)
    | T.T_MINUS_ASSIGN (ii)
    | T.T_PLUS_ASSIGN (ii)
        -> tag ii Punctuation

    | T.T_ASSIGN (ii)
        -> tag ii Punctuation

    | T.T_PLING (ii)
    | T.T_COLON (ii)
        -> tag ii Punctuation

    | T.T_ARROW ii -> tag ii Punctuation

    | T.T_OR (ii)
    | T.T_AND (ii)
    | T.T_BIT_OR (ii)
    | T.T_BIT_XOR (ii)
    | T.T_BIT_AND (ii)
    | T.T_EQUAL (ii)
    | T.T_NOT_EQUAL (ii)
    | T.T_STRICT_EQUAL (ii)
    | T.T_STRICT_NOT_EQUAL (ii)
    | T.T_LESS_THAN_EQUAL (ii)
    | T.T_GREATER_THAN_EQUAL (ii)
    | T.T_LESS_THAN (ii)
    | T.T_GREATER_THAN (ii)
    | T.T_LSHIFT (ii)
    | T.T_RSHIFT (ii)
    | T.T_RSHIFT3 (ii)
    | T.T_PLUS (ii)
    | T.T_MINUS (ii)
    | T.T_DIV (ii)
    | T.T_MULT (ii)
    | T.T_MOD (ii)
    | T.T_NOT (ii)
    | T.T_BIT_NOT (ii)
        -> tag ii Operator

    | T.T_INCR (ii)
    | T.T_DECR (ii)
        -> tag ii Punctuation

    | T.T_DELETE (ii)
    | T.T_TYPEOF (ii)
        -> tag ii Keyword

    | T.T_VOID (ii) ->
        tag ii TypeVoid

    | T.T_VIRTUAL_SEMICOLON (_ii)
        -> ()

    | T.TUnknown (_ii)
    | T.EOF (_ii)
      -> ()
  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)

  ()
