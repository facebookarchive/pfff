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
 * history:
 *  - LFS and code navigation
 *  - read jquery paper, prolog language for code query (and later semmle)
 *  - cmf --prolog for PHP
 *  - codequery generalization for ocaml
 *  - realization of how prolog interactive bdd is so much better than using
 *    ocaml and berkeley DB to answer simple questions
 *  - idea using prolog for more analysis, read bddbddb paper, doop, etc
 *  - need of "flowing" while reading plan9 kernel code:
 *    Where this indirect interrupt function is actually called? What
 *    can be the value in this field? etc
 * 
 *
 * related work:
 * - DOOP, bddbddb
 * - Andersen, steengaard, manuvir das, etc
 * - http://blog.jetbrains.com/idea/2009/08/analyzing-dataflow-with-intellij-idea/
 * - http://pag-www.gtisc.gatech.edu/chord/user_guide/datalog.html
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
