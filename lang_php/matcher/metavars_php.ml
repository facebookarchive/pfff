(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo? could want to remember the position in the pattern of the metavar
 * for error reporting ? so use a 'string Ast_php.wrap' ?
 *)
type mvar = string 

type metavars_binding = (mvar, Ast_php.any) Common.assoc

let empty_environment = []

(* bugfix: don't forget $, otherwise string like FBredirect would match
 * such regexp (the starting F) even if it's not a metavar at all
 *)
let metavar_regexp_string = 
  "\\([A-Z]\\([0-9]?_[A-Z]*\\)?\\)$"

let metavar_regexp  = Str.regexp metavar_regexp_string

(* 
 * Hacks abusing existing PHP constructs to encode extra constructions.
 * One day we will have a pattern_php_ast.ml that mimic mostly
 * ast_php.ml and extend it.
 *)
let is_metavar_name s = 
  s ==~ metavar_regexp

(* todo: replace this hack by allowing X->method(...) in php grammar *)
let is_metavar_variable_name s = 
  s =~ "V\\(_[A-Z]*\\)?"
