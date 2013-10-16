(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

open Parser_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module transforms certain tokens like '>>', normally a T_SR
 * into two TGREATER tokens which helps avoid using ugly tricks in the grammar
 * regarding generics.
 * 
 * This is similar to what we do for C/C++. 
 * See pfff/lang_cpp/parsing/parsing_hacks.ml for more information.
 * 
 * In Hack they maintain those different states (InToplevel, InFunction, 
 * InBlock, ...) in the lexer itself, I prefer for now to separate
 * concerns and do that entirely post-lexing (which introduces some performance
 * degradation, from 195s to parse www to 209s).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  stack: ctx list;
  misc: unit;
}
and ctx = 
 | Toplevel

 | ClassHeader
 | ClassBody
 | FunctionHeader
 | TypeHeader

 | UserAttribute

 | Block

(*****************************************************************************)
(* generics *)
(*****************************************************************************)

(* Split a single (assumed to be 2-chars wide) info and turn it
   into a (1-char) lhs and rhs. Used to convert `>>` into two `>`
*)
let split_two_char pi =
  let lhs = { pi with Parse_info.str = String.sub pi.Parse_info.str 0 1 } in
  let rhs = { pi with Parse_info.str = String.sub pi.Parse_info.str 1 1;
                     Parse_info.charpos = pi.Parse_info.charpos + 1;
                     Parse_info.column = pi.Parse_info.column + 1 } in
  (lhs, rhs)

let split_two_char_info i =
  let tok = match i.Parse_info.token with
    | Parse_info.OriginTok t -> t
    | _ -> failwith "Parse error..."
  in

  let lhspi, rhspi = split_two_char tok in
  let lhs = { Parse_info.token = Parse_info.OriginTok lhspi;
              Parse_info.transfo = Parse_info.NoTransfo
            } in
  let rhs = { Parse_info.token = Parse_info.OriginTok rhspi;
              Parse_info.transfo = Parse_info.NoTransfo
            } in
  (lhs, rhs)

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens2 xs =

  let rec aux env acc xs = 
    
    match xs with
    (* need an acc, to be tail recursive, otherwise get some stack overflow *)
    | [] -> List.rev acc

    (* '>>', maybe should be split in two tokens '>' '>' when in generic
     * context
     *)
    | T_SR ii::xs ->

      (match env.stack with
      (* type context, those are the only places where types allowed for 
       * now, which makes the job easier than in parsing_hacks_java.ml
       *)
      | (ClassHeader | ClassBody | TypeHeader | FunctionHeader)::_ ->
        let (lhs, rhs) = split_two_char_info ii in
        aux env (TGREATER rhs::TGREATER lhs::acc) xs
      | UserAttribute::rest ->
         aux { env with stack = rest } (T_SR ii::acc) xs
      | _ ->
        aux env (T_SR ii::acc) xs
      )

    | x::xs -> 
      let stack =
        (* quite similar to hack/lexing_modes.ml *)
        match x, env.stack with
       (* ugly: we check we are at toplevel because the keyword 'class' 
        * could be used in a different context as part of an XHP attribute
        * name, see ident_xhp_attr_name_atom rule in parser_php.mly
        *)
        | (T_CLASS _ | T_TRAIT _ | T_INTERFACE _),        Toplevel::rest ->
            ClassHeader::env.stack
        | (T_TYPE _ | T_NEWTYPE _),    Toplevel::rest  ->
            TypeHeader::env.stack
        | T_FUNCTION _, (Toplevel|ClassHeader)::rest ->
            FunctionHeader::env.stack
        | T_FUNCTION _, Block::rest ->
            FunctionHeader::env.stack

        (* also FunctionHeader because we can have attributes on parameters *)
        | T_SL _, (Toplevel | ClassBody | FunctionHeader)::rest ->
            UserAttribute::env.stack

        | TOBRACE ii, ClassHeader::rest ->
            ClassBody::rest
        (* subtle: do not do Block::env.stack here otherwise we will
         * not pop up enough to get back to a Toplevel context
         *)
        | TOBRACE ii, FunctionHeader::rest ->
            Block::rest

        | TOBRACE ii, _ ->
            Block::env.stack

        | (T_CURLY_OPEN _ | T_DOLLAR_OPEN_CURLY_BRACES _), _ ->
            Block::env.stack

        | TCBRACE ii, x::xs ->
            xs
        | TCBRACE ii, [] ->
            failwith (spf "unmaching closing brace at %s" 
                        (PI.string_of_info ii))

        | TSEMICOLON ii, (FunctionHeader|TypeHeader)::rest ->
            rest

        (* default case *)
        | _, st -> st
      in
      aux { env with stack } (x::acc) xs
  in
  aux {
    stack = [Toplevel];
    misc = ();
  } [] xs

    

let fix_tokens a =
  Common.profile_code "Parse_php.fix_tokens" (fun () -> fix_tokens2 a)
