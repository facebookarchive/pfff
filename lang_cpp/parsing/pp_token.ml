(* Yoann Padioleau
 * 
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes
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
module TH = Token_helpers_cpp
module Parser = Parser_cpp
module Ast = Ast_cpp

open Parser_cpp
open Token_views_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* cpp functions working at the token level. Cf cpp_ast_c for cpp functions
 * working at the AST level (which is very unusual but makes sense in 
 * the coccinelle context for instance).
 *  
 * Note that because I use a single lexer to work both at the C and cpp level
 * there are some inconveniencies. 
 * For instance 'for' is a valid name for a macro parameter and macro 
 * body, but is interpreted in a special way by our single lexer, and 
 * so at some places where I expect a TIdent I need also to
 * handle special cases and accept Tfor, Tif, etc at those places.
 * 
 * There are multiple issues related to those keywords incorrect tokens.
 * Those keywords can be:
 *   - (1) in the name of the macro as  in  #define inline
 *   - (2) in a parameter of the macro as in #define foo(char)   char x;
 *   - (3) in an argument to a macro call as in   IDENT(if);
 * Case 1 is easy to fix in define_ident.
 * Case 2 is easy to fix in define_parse where detect such toks in 
 * the parameter and then replace their occurence in the body in a Tident.
 * Case 3 is only an issue when the expanded token is not really use 
 * as usual but use for instance in concatenation as in  a ## if
 * when expanded. In the case the grammar this time will not be happy
 * so this is also easy to fix in cpp_engine.
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_cpp.verbose_parsing 

let pr2_pp s = 
  if !Flag.debug_pp
  then Common.pr2_once ("PP-" ^ s)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type define_body = (unit,string list) either * Parser_cpp.token list

(* ------------------------------------------------------------------------- *)
(* mimic standard.h *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Parsing and helpers of hints  *)
(*****************************************************************************)

(*****************************************************************************)
(* Expansion helpers *)
(*****************************************************************************)

(* Thanks to this function many stuff are not anymore hardcoded in ocaml code
 * (but they are now hardcoded in standard.h ...)
 *)
let rec (cpp_engine: (string , Parser.token list) assoc -> 
          Parser.token list -> Parser.token list) = fun env xs ->
  xs +> List.map (fun tok -> 
    match tok with
    | TIdent (s,i1) when List.mem_assoc s env -> Common.assoc s env
    | x -> [x]
  )
  +> List.flatten

(* ------------------------------------------------------------------------- *)
(* apply macro, using standard.h or other defs *)
(* ------------------------------------------------------------------------- *)

(* cpp-builtin part1, macro, using standard.h or other defs *)

(* no need to take care to not substitute the macro name itself
 * that occurs in the macro definition because the macro name is
 * after fix_token_define a TDefineIdent, no more a TIdent.
 *)
let rec apply_macro_defs defs xs = 

 let rec apply_macro_defs xs =
  match xs with
  | [] -> ()

  (* recognized macro of standard.h (or other) *)
  | PToken ({tok = TIdent (s,i1);_} as id)::Parenthised (xxs,info_parens)::xs 
      when Hashtbl.mem defs s -> 
      pr2_pp ("MACRO: found known macro = " ^ s);
      (match Hashtbl.find defs s with
      | Left (), bodymacro -> 
          pr2 ("macro without param used before parenthize, wierd: " ^ s);
          (* ex: PRINTP("NCR53C400 card%s detected\n" ANDP(((struct ... *)
          set_as_comment (Token_cpp.CppMacro) id;
          id.new_tokens_before <- bodymacro;
      | Right params, bodymacro -> 
          if List.length params = List.length xxs
          then
            let xxs' = xxs +> List.map (fun x -> 
              (tokens_of_paren_ordered x) +> List.map (fun x -> 
                TH.visitor_info_of_tok Ast.make_expanded x.tok
              )
            ) in
            id.new_tokens_before <-
              cpp_engine (Common.zip params xxs') bodymacro

          else begin
            pr2 ("macro with wrong number of arguments, wierd: " ^ s);
            id.new_tokens_before <- bodymacro;
          end;
          (* important to do that after have apply the macro, otherwise
           * will pass as argument to the macro some tokens that
           * are all TCommentCpp
           *)
          [Parenthised (xxs, info_parens)] +> 
            iter_token_paren (set_as_comment Token_cpp.CppMacro);
          set_as_comment Token_cpp.CppMacro id;

           

      );
      apply_macro_defs xs

  | PToken ({tok = TIdent (s,i1);_} as id)::xs 
      when Hashtbl.mem defs s -> 
      pr2_pp ("MACRO: found known macro = " ^ s);
      (match Hashtbl.find defs s with
      | Right params, bodymacro -> 
          pr2 ("macro with params but no parens found, wierd: " ^ s);
          (* dont apply the macro, perhaps a redefinition *)
          ()
      | Left (), bodymacro -> 
          (* special case when 1-1 substitution, we reuse the token *)
          (match bodymacro with
          | [newtok] -> 
              id.tok <- (newtok +> TH.visitor_info_of_tok (fun _ -> 
                TH.info_of_tok id.tok))

          | _ -> 
              set_as_comment Token_cpp.CppMacro id;
              id.new_tokens_before <- bodymacro;
          )
      );
      apply_macro_defs xs

  (* recurse *)
  | (PToken x)::xs -> apply_macro_defs xs 
  | (Parenthised (xxs, info_parens))::xs -> 
      xxs +> List.iter apply_macro_defs;
      apply_macro_defs xs

 in
 apply_macro_defs xs

(*****************************************************************************)
(* Extracting define_def from a standard.h  *)
(*****************************************************************************)

let rec define_parse xs = 
  match xs with
  | [] -> []
  | TDefine i1::TIdentDefine (s,i2)::TOParDefine i3::xs -> 
      let (tokparams, _, xs) = 
        xs +> Common.split_when (function TCPar _ -> true | _ -> false) in
      let (body, _, xs) = 
        xs +> Common.split_when (function TDefEOL _ -> true | _ -> false) in
      let params = 
        tokparams +> Common.map_filter (function
        | TComma _ -> None
        | TIdent (s, _) -> Some s
        | x -> error_cant_have x
        ) in
      let body = body +> List.map 
        (TH.visitor_info_of_tok Ast.make_expanded) in
      let def = (s, (Right params, body)) in
      def::define_parse xs

  | TDefine i1::TIdentDefine (s,i2)::xs -> 
      let (body, _, xs) = 
        xs +> Common.split_when (function TDefEOL _ -> true | _ -> false) in
      let body = body +> List.map 
        (TH.visitor_info_of_tok Ast.make_expanded) in
      let def = (s, (Left (), body)) in
      def::define_parse xs

  | TDefine i1::_ -> 
      raise Impossible
  | x::xs -> define_parse xs 
      

let extract_cpp_define xs = 
  let cleaner = xs +> List.filter (fun x -> 
    not (TH.is_comment x)
  ) in
  define_parse cleaner
