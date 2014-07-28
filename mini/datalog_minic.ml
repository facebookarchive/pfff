(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Generating dataflow-related datalog facts for mini C.
 * 
 * It's not that easy to translate the Java rules in DOOP to C. We could 
 * do a C -> Java translator, or think about how certain C features 
 * could be emulated in Java. This can give ideas.
 *
 * related work:
 * - DOOP, bddbddb
 * - Andersen, steengaard, manuvir das, etc
 * - http://blog.jetbrains.com/idea/2009/08/analyzing-dataflow-with-intellij-idea/
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type fact = string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let generate_facts _ast =
  []
