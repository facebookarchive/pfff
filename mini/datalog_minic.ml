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
open Common

open Ast_minic

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type fact = string

type env = {
  scope: string; (* qualifier, usually the current function *)

  (* for constant and globals *)
  globals: string list;
  structs: (string * struct_def) list;

  locals: string list;

  facts: fact list ref;
}

let add fact env =
  Common.push fact env.facts

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let variable_of_name _env name =
  spf "'%s'" (fst name)

let abstract_memory_location_of_constant _env name =
  spf "'_%d_'" (Parse_info.line_of_info (snd name))

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
let rec program env xs = 
  match xs with
  | [] -> ()
  | x::xs ->
    toplevel env x;
    program env xs

and toplevel env = function
  | StructDef _def -> ()
  | Global var ->
      add (spf "point_to(%s, %s)"
             (variable_of_name env var.v_name)
             (abstract_memory_location_of_constant env var.v_name)) env
  | Constant name ->
      add (spf "point_to(%s, %s)"
             (variable_of_name env name)
             (abstract_memory_location_of_constant env name)) env
  | FuncDef def -> func_def env def

and func_def _env _def =
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let generate_facts ast =
  let env = {
    scope = "_toplevel_";
    globals = [];
    structs = [];
    locals = [];
    facts = ref [];
  }
  in
  program env ast;
  !(env.facts)
