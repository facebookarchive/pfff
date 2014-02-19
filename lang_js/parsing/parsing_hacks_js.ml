(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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

module Ast = Ast_js
module T = Parser_js
module TH   = Token_helpers_js

(*****************************************************************************)
(* Lexer tricks *)
(*****************************************************************************)

let rparens_of_if toks = 
  let toks = Common.exclude TH.is_comment toks in

  let stack = ref [] in

  let rparens_if = ref [] in

  toks +> Common2.iter_with_previous_opt (fun prev x -> 
    (match x with
    | T.T_LPAREN _ -> 
        Common.push prev stack;
    | T.T_RPAREN info ->
        if !stack <> [] then begin
        let top = Common2.pop2 stack in
        (match top with
        | Some (T.T_IF _) -> 
            Common.push info rparens_if
        | _ ->
            ()
        )
        end
    | _ -> ()
    )
  );
  !rparens_if

(* UGLYYYYYYYYYYYYYYYYYYY. Better would be to read section 7.6.2.
*)
let fix_tokens xs =

  let rparens_if = rparens_of_if xs in
  let hrparens_if = Common.hashset_of_list rparens_if in

  match xs with
  | [] -> []
  | y::ys ->
      let res = ref [] in
      Common.push y res;
      let rec aux prev f xs = 
        match xs with
        | [] -> ()
        | e::l ->
            if TH.is_comment e
            then begin 
              Common.push e res;
              aux prev f l
            end else begin
              f prev e;
              aux e f l
            end
      in
      let f = (fun prev x ->
        match prev, x with
        | (T.T_LCURLY _ | T.T_SEMICOLON _ | T.T_VIRTUAL_SEMICOLON _), 
        T.T_RCURLY _ ->
            Common.push x res;
            (* also one after ? *)
(*            Common.push2 (T.T_VIRTUAL_SEMICOLON (Ast.fakeInfo ())) res; *)

        | _, T.T_RCURLY _ ->
            let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
            Common.push (T.T_VIRTUAL_SEMICOLON fake) res;
            Common.push x res;
            (* also one after ? *)
(*            Common.push2 (T.T_VIRTUAL_SEMICOLON (Ast.fakeInfo ())) res; *)
            
        | (T.T_SEMICOLON _ | T.T_VIRTUAL_SEMICOLON _),
            T.EOF _ ->
            Common.push x res;
        | _, T.EOF _ ->
            let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
            Common.push (T.T_VIRTUAL_SEMICOLON fake) res;
            Common.push x res;


        | T.T_RCURLY _, 
            (T.T_IDENTIFIER _ | 
             T.T_IF _ | T.T_VAR _ | T.T_FOR _ | T.T_RETURN _ |
             T.T_SWITCH _ |
             T.T_FUNCTION _ | T.T_THIS _ |
             T.T_BREAK _ | 
             T.T_NEW _

            ) 
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push x res;

        (* this is valid only if the RPAREN is not the closing paren
         * of a if
         *)
        | T.T_RPAREN info, 
            (T.T_VAR _ | T.T_IF _ | T.T_THIS _ | T.T_FOR _ | T.T_RETURN _ |
             T.T_IDENTIFIER _ | T.T_CONTINUE _ 
            ) when not (Hashtbl.mem hrparens_if info)
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push x res;


        | T.T_RBRACKET _, 
            (T.T_FOR _ | T.T_IF _ | T.T_VAR _ | T.T_IDENTIFIER _)
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push x res;


        | (T.T_IDENTIFIER _ | T.T_NULL _ | T.T_STRING _ | T.T_REGEX _
            | T.T_FALSE _ | T.T_TRUE _
          ), 
              (T.T_VAR _ | T.T_IDENTIFIER _ | T.T_IF _ | T.T_THIS _ |
                  T.T_RETURN _ | T.T_BREAK _ | T.T_ELSE _
              )
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push x res;

        | _, _ ->        
            Common.push x res;
      )
      in
      aux y f ys;
      List.rev !res
