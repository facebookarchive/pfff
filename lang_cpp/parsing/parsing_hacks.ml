(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011 Facebook
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

module Flag = Flag_parsing_cpp
module Ast = Ast_cpp

module TH = Token_helpers_cpp
module Parser = Parser_cpp

open Parser_cpp
open Token_views_cpp

open Parsing_hacks_lib

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module tries to detect some cpp, c, or c++ idioms so that we can
 * parse as-is files by adjusting or commenting some tokens.
 * Sometime we use some indentation information, sometimes we
 * do some kind of lalr(k) by finding patterns. We often try to
 * work on a better token representation, like ifdef-paren-ized, brace-ized,
 * paren-ized, so that we can pattern-match more easily
 * complex patterns (cf token_views_cpp.ml).
 * We also try to get more contextual information such as whether the
 * token is in an initializer because many patterns are different
 * depending on context.
 * 
 * Example of cpp idioms:
 *  - if 0 for commenting stuff (not always code, sometimes just real comments)
 *  - ifdef old version
 *  - ifdef funheader
 *  - ifdef statements, ifdef expression, ifdef-mid
 *  - macro toplevel (with or without a trailing ';')
 *  - macro foreach
 *  - macro higher order
 *  - macro declare
 *  - macro debug
 *  - macro no ';'
 *  - macro string, and macro function string taking param and ##
 *  - macro attribute
 * 
 * Example of typedef idioms:
 *  - x * y
 * 
 * Example of c++ idioms:
 *  - x<...> for templates. People rarely do x < y > z to express 
 *    relational expressions, so a < followed later by a > is probably a
 *    template.
 * 
 * Cf the TMacroXxx in parser_c.mly and MacroXxx in ast_c.ml

 * We also do other stuff involving cpp like expanding macros,
 * and we try parse define body by finding the end of define virtual 
 * end-of-line token. But now most of the code is actually in pp_token.ml
 * It is related to what is in the yacfe configuration file (e.g. standard.h)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_pp_stuff xs = 
  let rec aux xs = 
    match xs with
    | [] -> []
    | x::xs -> 
        (match x.t with
        | tok when TH.is_comment tok -> aux xs
        (* don't want drop the define, or if drop, have to drop
         * also its body otherwise the line heuristics may be lost
         * by not finding the TDefine in column 0 but by finding
         * a TDefineIdent in a column > 0
         *)
        | Parser.TDefine _ -> 
            x::aux xs
        | tok when TH.is_pp_instruction tok -> aux xs
        | _ -> x::aux xs
        )
  in
  aux xs
          
let insert_virtual_positions l =
  let strlen x = String.length (Ast.str_of_info x) in
  let rec loop prev offset = function
      [] -> []
    | x::xs ->
        let ii = TH.info_of_tok x in
        let inject pi =
          TH.visitor_info_of_tok (function ii -> Ast.rewrap_pinfo pi ii) x in
        match Ast.pinfo_of_info ii with
          Parse_info.OriginTok pi ->
            let prev = Ast.parse_info_of_info ii in
            x::(loop prev (strlen ii) xs)
        | Parse_info.ExpandedTok (pi,_, _) ->
            inject (Parse_info.ExpandedTok (pi, prev,offset)) ::
            (loop prev (offset + (strlen ii)) xs)
        | Parse_info.FakeTokStr (s,_) ->
            inject (Parse_info.FakeTokStr (s, (Some (prev,offset)))) ::
            (loop prev (offset + (strlen ii)) xs)
        | Parse_info.Ab -> failwith "abstract not expected" in
  let rec skip_fake = function
      [] -> []
    | x::xs ->
        let ii = TH.info_of_tok x in
        match Ast.pinfo_of_info ii with
          Parse_info.OriginTok pi ->
            let prev = Ast.parse_info_of_info ii in
            x::(loop prev (strlen ii) xs)
        | _ -> x::skip_fake xs in
  skip_fake l

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

(* 
 * Main entry point for the token reclassifier which generates "fresh" tokens.
 * 
 * The order of the rules is important, if you put the action heuristic first,
 * then because of ifdef, can have not closed paren
 * and so may believe that higher order macro 
 * and it will eat too much tokens. So important to do 
 * first the ifdef heuristic.
 * 
 * I recompute multiple times cleaner cos the mutable
 * can have be changed and so may have more comments
 * in the token original list.
 *)
let fix_tokens2 ~macro_defs tokens = 
  let tokens2 = ref (tokens +> acc_map mk_token_extended) in
  
  begin 

    (* ifdef *)
    let cleaner = !tokens2 +> List.filter (fun x -> 
      not (TH.is_comment x.t) (* could filter also #define/#include *)
    ) in
    let ifdef_grouped = mk_ifdef cleaner in
    Parsing_hacks_pp.find_ifdef_funheaders ifdef_grouped;
    Parsing_hacks_pp.find_ifdef_bool       ifdef_grouped;
    Parsing_hacks_pp.find_ifdef_mid        ifdef_grouped;


    (* macro part 1 *)
    let cleaner = !tokens2 +> filter_pp_stuff in

    let paren_grouped = mk_parenthised  cleaner in
    Pp_token.apply_macro_defs 
      macro_defs
      paren_grouped;
    (* because the before field is used by apply_macro_defs *)
    tokens2 := rebuild_tokens_extented !tokens2; 

    (* tagging contextual info (InFunc, InStruct, etc). Better to do
     * that after the "ifdef-simplification" phase.
     *)
    let cleaner = !tokens2 +> List.filter (fun x -> 
      not (TH.is_comment x.t) (* could filter also #define/#include *)
    ) in

    (* done on brace_grouped but actually modifies tokens2 *)
    let brace_grouped = mk_braceised cleaner in
    Token_views_cpp.set_context_tag   brace_grouped;

    (* macro part2 *)
    let cleaner = !tokens2 +> filter_pp_stuff in

    let paren_grouped      = mk_parenthised  cleaner in
    let line_paren_grouped = mk_line_parenthised paren_grouped in
    Parsing_hacks_pp.find_define_init_brace_paren paren_grouped;
    Parsing_hacks_pp.find_string_macro_paren paren_grouped;
    Parsing_hacks_pp.find_macro_lineparen    line_paren_grouped;
    Parsing_hacks_pp.find_macro_paren        paren_grouped;

    (* todo: find template <> symbols (need to be done before
     * typedef heuristics *)

    (* todo: typedefs *)

    (* c++ stuff *)
    Parsing_hacks_cpp.find_view_filtered_tokens cleaner;
    Parsing_hacks_cpp.find_view_parenthized paren_grouped;
    Parsing_hacks_cpp.find_view_parenthized2 paren_grouped;
    Parsing_hacks_cpp.find_view_line_paren line_paren_grouped;
    Parsing_hacks_cpp.find_view_filtered_tokens_bis cleaner;

    (* actions *)
    let cleaner = !tokens2 +> filter_pp_stuff in
    let paren_grouped = mk_parenthised  cleaner in
    Parsing_hacks_pp.find_actions  paren_grouped;

    insert_virtual_positions (!tokens2 +> acc_map (fun x -> x.t))
  end

let fix_tokens ~macro_defs a = 
  Common.profile_code "C++ parsing.fix_tokens" (fun () -> 
    fix_tokens2 ~macro_defs a)
