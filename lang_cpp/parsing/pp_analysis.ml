(* Yoann Padioleau
 * 
 * Copyright (C) 2009 University of Urbana Champaign
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

open Oset 

open Parser_cpp

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * Is this module make all the tricks used in parsing_hacks and 
 * most definitions in standard.h obsolete ? It depends. In a 
 * static analysis context we want to be accurate, and so expand
 * all the code that will make our type/callgraph analysis simpler.
 * So we want to expand many macros, based on heuristics in this file.
 * In a transformation context, we want to let the programmer
 * match over certain constructs such as declarator, iterator, 
 * macro_field, etc, and in this case we want to parse as-is.
 * 
 * What could be done is that some of the analysis performed in this
 * file could then be injected in parsing_hacks, for instance via
 * hints, to make the parse as-is job easier too.
 * 
 * 
 *
 * todo: right now I find dangerous macro based on ## and go upward
 * to also include calling macros. But this dangerous macro itself
 * may use other macros that looks ok but that should also be expanded
 * because it defines some entities. So also recurse downward ?
 * 
 * todo? do analysis a la Astec ? try infer the meaning of the macro
 * from its body but also from its context of use ? Can then
 * do a taxonomy of macro ? not just foreach or declarator but
 * polymorphic function (e.g. MAX), type generator, etc. Cf astec paper 
 * or Ernst cpp study paper ?
 * 
 *)


(* TODO: copy code in parsing_c/ *)

(*****************************************************************************)
(* Main entry point  *)
(*****************************************************************************)

(*
let extract_dangerous_macro xs =
  raise Todo
*)
