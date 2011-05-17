(* Yoann Padioleau
 * 
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes
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

module TH = Token_helpers_cpp

open Parser_cpp
open Token_views_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* cpp functions working at the token level. Cf cpp_ast_c for cpp functions
 * working at the AST level (which is very unusual but makes sense in 
 * the coccinelle context for instance).
 *  
 * Note that as I use a single lexer  to work both at the C and cpp level
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
 * 
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_cpp.verbose_parsing 

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type define_body = (unit,string list) either * Parser_cpp.token list

