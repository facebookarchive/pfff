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

open Ast_js
open Parser_js (* the tokens *)

module V = Visitor_js
module Ast = Ast_js
module TH = Token_helpers_js
module PI = Parse_info
open Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* todo: reuse pfff/matcher/ code *)

(*****************************************************************************)
(* unparsing by using the tokens *)
(*****************************************************************************)

let is_not_in_ast = function
  | TComment _ | TCommentSpace _ | TCommentNewline _ 
      -> true
  | _ -> false
let is_in_ast tok = not (is_not_in_ast tok)

let is_in_between_some_remove prev_tok cur_tok = 
  match (TH.info_of_tok prev_tok).transfo, 
        (TH.info_of_tok cur_tok).transfo with
  | Remove, Remove -> true
  | _ -> false

let is_behind_a_remove_or_replace prev_tok cur_tok = 
  match (TH.info_of_tok prev_tok).transfo, 
        (TH.info_of_tok cur_tok).transfo with
  | (Remove | Replace _), _ -> true
  | _ -> false

let (string_of_program2_using_tokens: Parse_js.program_and_tokens -> string) = 
 fun (_ast, toks) ->


   (* for some of the processing below, it is convenient to enclose
    * the list of tokens with some fake tokens so that the special
    * case on the edges does not have to be handled.
    *)
   let fake_tok = 
     let info =  {
       Parse_info.token = Parse_info.OriginTok { Parse_info.
         charpos = 0; 
         str     = "";

         (* info filled in a post-lexing phase, cf Parse_sql.tokens *)
         line = -1; 
         column = -1; 
         file = "";
       };
       transfo = Parse_info.NoTransfo;
     }
     in
     Parser_js.T_SEMICOLON (info)
   in

  Common2.with_open_stringbuf (fun (_pr_with_nl, buf) ->
    let pp s = 
      Buffer.add_string buf s 
    in

    let pp_tok tok = 
      match (TH.info_of_tok tok).Parse_info.token with
      | Parse_info.OriginTok _ -> 
          pp (TH.str_of_tok tok);

      | Parse_info.ExpandedTok _ -> 
          ()

      | Parse_info.Ab | Parse_info.FakeTokStr _ -> raise Impossible
    in
    let pp_add toadd = 
      match toadd with
      | AddStr s -> pp s
      | AddNewlineAndIdent -> 
          raise Todo
    in
    

      let toks = [fake_tok] ++ toks ++ [fake_tok] in
      
      let (toks_ast_with_comment_attached, trailing_comments) = 
        Common2.group_by_post (fun tok -> is_in_ast tok) toks
      in
      (* the last comments should be attached to the last fake_tok *)
      assert(null trailing_comments);

      toks_ast_with_comment_attached +> Common2.iter_with_previous 
          (fun (comments_prev, tok_prev) (comments, tok)  ->

            if is_in_between_some_remove tok_prev tok 
             (* TODO: this is ok only for certain tokens, such
              * as comma
              *  || is_behind_a_remove_or_replace tok_prev tok
              *)
            then () 
            else comments +> List.iter pp_tok;

            let info = TH.info_of_tok tok in

            (match (TH.info_of_tok tok).PI.token, info.transfo with
            | ExpandedTok _, NoTransfo -> () 
            | ExpandedTok _, 
                (Remove | Replace _ | AddAfter _ | AddBefore _) ->
                failwith "Can't do transformation on expanded Tok"

            | Ab, _ -> raise Impossible
            | FakeTokStr _, _ -> raise Impossible

            | OriginTok _, _ -> 

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
  )

