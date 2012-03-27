(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
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

open Ast_php
open Parse_info
module Ast = Ast_php
module V = Visitor_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See https://github.com/facebook/pfff/wiki/Sgrep *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* right now only Expr and Stmt are actually supported *)
type pattern = Ast_php.any

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* We can't have Flag_parsing_php.case_sensitive set to false here
 * because metavariables in sgrep patterns are in uppercase
 * and we don't want to lowercase them.
 * 
 * We actually can't use Flag_parsing_php.case_sensitive at all
 * because we also want the Foo() pattern to match foo() code
 * so we have anyway to do some case insensitive string
 * comparisons in php_vs_php.ml
 *)
let parse str =
  Common.save_excursion Flag_parsing_php.sgrep_mode true (fun () ->
    Parse_php.any_of_string str
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let sgrep ?(case_sensitive=false) ~hook pattern file =
  let (ast2) = 
    try 
      Parse_php.parse file +> fst
    with Parse_php.Parse_error err ->
      Common.pr2 (spf "warning: parsing problem in %s" file);
      []
  in
  let ast = Parse_php.program_of_program2 ast2 in

  (* coupling: copy paste with lang_php/matcher/spatch_php.ml *)
  let hook = 
    match pattern with
    (* bugfix: if we have an ExprVar, then a pattern such as 
     * $foo->method() will not match expressions such as 
     * $foo->method()->method because the full ExprVar will not
     * match. The pb is that we need not only a match_e_e but also
     * a match_v_v with the same visitor. For each recursive
     * types inside a recursive types, we need to do that
     * (for now lvalue and xhp_html).
     *)
    | Expr (Lv pattern_var) ->
        { V.default_visitor with
          V.klvalue = (fun (k, _) x ->
            let matches_with_env =  
              Matching_php.match_v_v pattern_var x
            in 
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_parsing_php.ii_of_any (Lvalue x) in
              matches_with_env +> List.iter (fun env -> 
                hook env matched_tokens
              )
            end
          );
        }
    | Expr (XhpHtml xhp) ->
        { V.default_visitor with
          V.kxhp_html = (fun (k, _) x ->
            let matches_with_env =  
              Matching_php.match_xhp_xhp xhp x
            in 
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_parsing_php.ii_of_any (XhpHtml2 x) in
              matches_with_env +> List.iter (fun env -> 
                hook env matched_tokens
              )
            end
          );
        }

    | Expr pattern_expr ->
        { V.default_visitor with
          V.kexpr = (fun (k, _) x ->
            let matches_with_env =  
              Matching_php.match_e_e pattern_expr  x
            in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_parsing_php.ii_of_any (Expr x) in
              matches_with_env +> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }
    | Stmt2 pattern ->
        { V.default_visitor with
          V.kstmt = (fun (k, _) x ->
            let matches_with_env =  
              Matching_php.match_st_st pattern x
            in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_parsing_php.ii_of_any (Stmt2 x) in
              matches_with_env +> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }

    | Toplevel pattern ->
        { V.default_visitor with
          V.ktop = (fun (k, _) x ->
            let matches_with_env =  
              Matching_php.match_top_top pattern x
            in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_parsing_php.ii_of_any (Toplevel x) in
              matches_with_env +> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }
          
    | _ -> failwith (spf "pattern not yet supported:" ^ 
                        Export_ast_php.ml_pattern_string_of_any pattern)
  in
  (* opti ? dont analyze func if no constant in it ?*)
  Common.save_excursion Php_vs_php.case_sensitive case_sensitive (fun() ->
    (V.mk_visitor hook) (Program ast)
  )

