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
module Parser = Parser_cpp
module TH = Token_helpers_cpp
module TV = Token_views_cpp
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module tries to detect some CPP, C, or C++ idioms so that we can
 * parse as-is files by adjusting or commenting some tokens.
 * 
 * Sometime we use some name conventions, sometimes indentation information, 
 * sometimes we do some kind of lalr(k) by finding patterns. We often try to
 * work on a better token representation, like ifdef-paren-ized, brace-ized,
 * paren-ized, so that we can pattern-match more easily
 * complex idioms (cf token_views_cpp.ml).
 * We also try to get more contextual information such as whether the
 * token is in an initializer because many idioms are different
 * depending on the context.
 * 
 * Example of CPP idioms:
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
 * See the TIdent_MacroXxx in parser_cpp.mly and MacroXxx in ast_cpp.ml
 * 
 * We also do other stuff involving CPP like expanding macros,
 * and we try to parse define body by finding the end of define virtual
 * end-of-line token. But now most of the code is actually in pp_token.ml
 * It is related to what is in the yacfe configuration file (e.g. standard.h)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_comment_stuff xs =
  xs +> List.filter (fun x -> not (TH.is_comment x.TV.t))
          
let insert_virtual_positions l =
  let strlen x = String.length (PI.str_of_info x) in
  let rec loop prev offset = function
      [] -> []
    | x::xs ->
        let ii = TH.info_of_tok x in
        let inject pi =
          TH.visitor_info_of_tok (function ii -> Ast.rewrap_pinfo pi ii) x in
        match ii.Parse_info.token with
          Parse_info.OriginTok pi ->
            let prev = Parse_info.token_location_of_info ii in
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
        match ii.Parse_info.token with
          Parse_info.OriginTok pi ->
            let prev = Parse_info.token_location_of_info ii in
            x::(loop prev (strlen ii) xs)
        | _ -> x::skip_fake xs in
  skip_fake l

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)
(* 
 * Main entry point for the token reclassifier which generates "fresh" tokens.
 * 
 * The order of the rules is important. For instance if you put the 
 * action heuristic first, then because of ifdef, can have not closed paren
 * and so may believe that higher order macro
 * and it will eat too much tokens. So important to do
 * first the ifdef heuristic.
 * 
 * Note that the functions below work on a list of token_extended
 * or on views on top of a list of token_extended. token_extended
 * contains mutable field which explains the (ugly but working) imperative
 * style of the code below.
 * 
 * I recompute multiple times 'cleaner' cos the mutable
 * can have be changed and so may have more comments
 * in the token original list.
 *)
let fix_tokens2 ~macro_defs tokens = 
  let tokens2 = ref (tokens +> Common2.acc_map TV.mk_token_extended) in
  
  (* ifdef *)
  let cleaner = !tokens2 +> filter_comment_stuff in
  
  let ifdef_grouped = TV.mk_ifdef cleaner in
  Parsing_hacks_pp.find_ifdef_funheaders ifdef_grouped;
  Parsing_hacks_pp.find_ifdef_bool       ifdef_grouped;
  Parsing_hacks_pp.find_ifdef_mid        ifdef_grouped;
  
  (* macro part 1 *)
  let cleaner = !tokens2 +> Parsing_hacks_pp.filter_pp_or_comment_stuff in
  
  (* find '<' '>' template symbols. We need that for the typedef
   * heuristics. We actually need that even for the paren view 
   * which is wrong without it.
   * 
   * todo? expand macro first? some expand to lexical_cast ...
   * but need correct parenthized view to expand macros => deadlock.
   *)
  Parsing_hacks_cpp.find_template_inf_sup cleaner;
  
  let paren_grouped = TV.mk_parenthised  cleaner in
  Pp_token.apply_macro_defs macro_defs paren_grouped;
  
  (* because the before field is used by apply_macro_defs *)
  tokens2 := TV.rebuild_tokens_extented !tokens2; 
  
  (* could filter also #define/#include *)
  let cleaner = !tokens2 +> filter_comment_stuff in
  
  (* tagging contextual info (InFunc, InStruct, etc). Better to do
   * that after the "ifdef-simplification" phase.
   * 
   * TODO: use multi view here instead, and make it more complete.
   *)
  let brace_grouped = TV.mk_braceised cleaner in
  Token_views_context.set_context_tag   brace_grouped;
  
  (* macro part 2 *)
  let cleaner = !tokens2 +> Parsing_hacks_pp.filter_pp_or_comment_stuff in
  
  let paren_grouped      = TV.mk_parenthised  cleaner in
  let line_paren_grouped = TV.mk_line_parenthised paren_grouped in
  Parsing_hacks_pp.find_define_init_brace_paren paren_grouped;
  Parsing_hacks_pp.find_string_macro_paren paren_grouped;
  Parsing_hacks_pp.find_macro_lineparen    line_paren_grouped;
  Parsing_hacks_pp.find_macro_paren        paren_grouped;
  
  let multi_grouped = TV.mk_multi cleaner in

  (* todo: at some point we need to remove that and use
   * a better filter_for_typedef that also
   * works on the nested template arguments.
   *)
  Parsing_hacks_cpp.find_template_commentize multi_grouped;
  let cleaner = !tokens2 +> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  (* must be done before the qualifier filtering *)
  Parsing_hacks_cpp.find_constructor_outside_class cleaner;

  Parsing_hacks_cpp.find_qualifier_commentize cleaner;
  let cleaner = !tokens2 +> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  let multi_grouped = TV.mk_multi cleaner in
  Parsing_hacks_cpp.set_context_tag_cplus multi_grouped;

  Parsing_hacks_cpp.find_constructor cleaner;
  let xxs = Parsing_hacks_cpp.filter_for_typedef multi_grouped in
  Parsing_hacks_typedef.find_typedefs xxs;

  (* must be done after the typedef inference *)
  Parsing_hacks_cpp.find_constructed_object_and_more cleaner;
  
  Parsing_hacks_cpp.reclassify_tokens_before_idents_or_typedefs multi_grouped;
  
  insert_virtual_positions (!tokens2 +> Common2.acc_map (fun x -> x.TV.t))


let fix_tokens ~macro_defs a = 
  Common.profile_code "C++ parsing.fix_tokens" (fun () -> 
    fix_tokens2 ~macro_defs a)
