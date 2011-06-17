(* Yoann Padioleau
 * 
 * Copyright (C) 2008, 2009 University of Urbana Champaign
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

open Ast_cpp

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = 
  Common.mk_pr2_wrappers Flag_parsing_cpp.verbose_pp_ast
let pr2_debug, pr2_debug_once = 
  Common.mk_pr2_wrappers Flag_parsing_cpp.debug_pp_ast

(*****************************************************************************)
(* Cpp Ast Manipulations *)
(*****************************************************************************)

(*
 * cpp-include-expander-builtin.
 * 
 * alternative1: parse and call cpp tour a tour. So let cpp work at 
 *  the token level. That's what most tools do.
 * alternative2: apply cpp at the very end. Process that go through ast
 *  and do the stuff such as #include,  macro expand, 
 *  ifdef but on the ast!
 * 
 * But need keep those info in ast at least, even bad
 * macro for instance, and for parse error region ? maybe can 
 * get another chance ?
 * I think it's better to do the cpp-include-expander in a different step
 * rather than embedding it in the parser. The parser is already too complex. 
 * Also keep with the tradition to try to parse as-is.
 * 
 * todo? but maybe could discover new info that could help reparse
 * the ParseError in original file. Try again parsing it by
 * putting it in a minifile ? 
 * 
 * 
 * todo? maybe can do some pass that work at the ifdef level and for instance
 * try to paren them, so have in Ast some stuff that are not
 * present at parsing time but that can then be constructed after
 * some processing (a little bit like my type for expression filler,
 * or position info filler, or include relative position filler).
 * 
 * ??add such info about what was done somewhere ? could build new
 * ??ast each time but too tedious (maybe need delta-programming!)
 *
 * todo? maybe change cpp_ast_c to go deeper on local "" ?
 * 
 * 
 * TODO: macro expand, 
 * TODO: handle ifdef
 * 
 * 
 * 
 * cpp_ifdef_statementize: again better to separate concern and in parser
 *  just add the directives in a flat way (IfdefStmt) and later do more
 *  processing and transform them in a tree with some IfdefStmt2.
 *)



