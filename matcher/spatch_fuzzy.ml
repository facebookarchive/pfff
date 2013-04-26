(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * See https://github.com/facebook/pfff/wiki/Spatch
 * 
 * Here is an example of a spatch file:
 * 
 *    foo(2, 
 * -      bar(2)
 * +      foobar(4)
 *       )
 * 
 * This will replace all calls to bar(2) by foobar(4) when
 * the function call is the second argument of a call to
 * foo where its first argument is 2.
 * 
 * 
 * note: can we produce syntactically incorrect code? Yes ...
 * 
 * less: mostly copy paste of spatch_php.ml
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

type pattern = Ast_fuzzy.trees

type line_kind = 
  | Context
  | Plus of string
  | Minus

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* 
 * Algorithm to parse a spatch file:
 *  - take lines of the file, index the lines
 *  - replace the + lines by an empty line and remember in a line_env
 *    the line and its index
 *  - remove the - in the first column and remember in a line_env
 *    that is was a minus line
 *  - unlines the filtered lines into a new string 
 *  - call the parser on this new string
 *  - go through all tokens and adjust its transfo field using the
 *    information in line_env
 *)

let parse file =
  raise Todo

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let spatch pattern file =
  raise Todo

