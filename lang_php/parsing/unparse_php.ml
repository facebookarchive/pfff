(*s: unparse_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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
(*e: Facebook copyright *)

open Common 

open Ast_php 
open Parser_php (* the tokens *)

module V = Visitor_php
module Ast = Ast_php

module TH = Token_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * There are multiple ways to unparse PHP code. 
 * One can iterating over the AST, and print its leaves, but 
 * comments and spaces are not in the AST right now which require 
 * extra code. 
 * One can iterate over the tokens, where comments and spaces are normal
 * citizens, but this can be too low level.
 * 
 * related: the sexp/json "exporters".
 * 
 * todo? Want to put this module in parsing_php/ 
 * it does not have to be here, but maybe simpler
 * to put it here so have   basic parser/unparser
 * together.
 *)

(*****************************************************************************)
(* unparsing by using AST visitor *)
(*****************************************************************************)

let string_of_program2 ast2 = 
  Common.with_open_stringbuf (fun (_pr_with_nl, buf) ->
    let pp s = 
      Buffer.add_string buf s
    in
    let cur_line = ref 1 in

    pp "<?php";
    pp "\n"; 
    incr cur_line;

    let hooks = { V.default_visitor with
      V.kinfo = (fun (k, _) info ->
        match info.pinfo with
        | Parse_info.OriginTok p ->
            let line = p.Parse_info.line in 
            if line > !cur_line
            then begin
              (line - !cur_line) +> Common.times (fun () -> pp "\n"); 
              cur_line := line;
            end;

            let s =  p.Parse_info.str in
            pp s; pp " ";
        | Parse_info.FakeTokStr (s, _opt) ->
            pp s; pp " ";
            if s = ";" 
            then begin
              pp "\n";
              incr cur_line;
            end

        | Parse_info.Ab
          ->
            ()
        | Parse_info.ExpandedTok _ ->
            raise Todo
      );
    }
    in
    
    ast2 +> List.iter (fun (top, infos) ->
      (V.mk_visitor hooks) (Toplevel top)
    )
  )

let mk_unparser_visitor pp = 
  let hooks = { V.default_visitor with
    V.kinfo = (fun (k, _) info ->
      match info.pinfo with
      | Parse_info.OriginTok p ->
          let s =  p.Parse_info.str in
          pp s; (* pp " "; *)
      | Parse_info.FakeTokStr (s, _opt) ->
          pp s; pp " ";
          if s = ";" || s = "{" || s = "}"
          then begin
            pp "\n";
          end

      | Parse_info.Ab
        ->
          ()
      | Parse_info.ExpandedTok _ -> raise Todo
    );
  }
  in
  (V.mk_visitor hooks)
    

let string_of_infos ii = 
  (* todo: keep space, keep comments *)
  ii |> List.map (fun info -> Ast.str_of_info info) |> Common.join ""


let string_of_expr_old e = 
  let ii = Lib_parsing_php.ii_of_any (Expr e) in
  string_of_infos ii

let string_of_any any = 
  Common.with_open_stringbuf (fun (_pr_with_nl, buf) ->
    let pp s = Buffer.add_string buf s in
    (mk_unparser_visitor pp) any
  )

(*****************************************************************************)
(* unparsing by using the tokens *)
(*****************************************************************************)

let is_not_in_ast = function
  | T_WHITESPACE _
  | T_COMMENT _ | T_DOC_COMMENT _ 
  | TComment _ | TCommentSpace _ | TCommentNewline _ 
      -> true
  | _ -> false
let is_in_ast tok = not (is_not_in_ast tok)

let is_in_between_some_remove prev_tok cur_tok = 
  match (TH.info_of_tok prev_tok).transfo, 
        (TH.info_of_tok cur_tok).transfo with
  | Remove _, Remove _ -> true
  | _ -> false

let is_behind_a_remove_or_replace prev_tok cur_tok = 
  match (TH.info_of_tok prev_tok).transfo, 
        (TH.info_of_tok cur_tok).transfo with
  | (Remove _ | Replace _), _ -> true
  | _ -> false

let string_of_program2_using_tokens 
 ?(remove_space_after_removed = true)
  ast2 =

   (* for some of the processing below, it is convenient to enclose
    * the list of tokens with some fake tokens so that the special
    * case on the edges does not have to be handled.
    *)
   let fake_tok = 
     let e = Parse_php.expr_of_string "1" in
     let ii = Lib_parsing_php.ii_of_any (Expr e) in
     match ii with
     | [info] -> Parser_php.T_ECHO (Ast.rewrap_str "" info)
     | _ -> raise Impossible
   in

  Common.with_open_stringbuf (fun (_pr_with_nl, buf) ->
    let pp s = 
      Buffer.add_string buf s 
    in

    let pp_tok tok = 
      match TH.pinfo_of_tok tok with
      | Parse_info.OriginTok _ -> 
          pp (TH.str_of_tok tok);

      | Parse_info.ExpandedTok _ -> 
          ()

      | Parse_info.Ab _ | Parse_info.FakeTokStr _ -> raise Impossible
    in
    let pp_add toadd = 
      match toadd with
      | AddStr s -> pp s
      | AddNewlineAndIdent -> 
          raise Todo
    in
    
    ast2 |> List.iter (fun (ast, (s, toks)) ->

      let toks = [fake_tok] ++ toks ++ [fake_tok] in
      
      let (toks_ast_with_comment_attached, trailing_comments) = 
        Common.group_by_post (fun tok -> is_in_ast tok) toks
      in
      (* the last comments should be attached to the last fake_tok *)
      assert(null trailing_comments);

      toks_ast_with_comment_attached |> Common.iter_with_previous 
          (fun (comments_prev, tok_prev) (comments, tok)  ->

            if is_in_between_some_remove tok_prev tok 
             (* TODO: this is ok only for certain tokens, such
              * as comma
              *  
              *)
              || (is_behind_a_remove_or_replace tok_prev tok &&
                  remove_space_after_removed)
            then () 
            else comments |> List.iter pp_tok;

            let info = TH.info_of_tok tok in

            (match TH.pinfo_of_tok tok, info.transfo with
            | Parse_info.ExpandedTok _, NoTransfo -> () 
            | Parse_info.ExpandedTok _, 
                (Remove | Replace _ | AddAfter _ | AddBefore _) ->
                failwith "Can't do transformation on expanded Tok"

            | Parse_info.Ab, _ -> raise Impossible
            | Parse_info.FakeTokStr _, _ -> raise Impossible

            | Parse_info.OriginTok _, _ -> 

                (match info.transfo with
                | NoTransfo -> 
                    pp_tok tok
                | Remove -> 
                    ()
                | Replace toadd ->
                    pp_add toadd
                      
                | AddAfter toadd ->
                    pp_tok tok;
                    pp_add toadd;
                | AddBefore toadd ->
                    pp_add toadd;
                    pp_tok tok;
                )
            )
          )
    );
  )
  
(*e: unparse_php.ml *)
