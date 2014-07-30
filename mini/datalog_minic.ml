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
 * could be emulated in Java. This can give ideas. But right now
 * reading the seminal paper on pointer analysis for C (Andersen thesis)
 * is the best approach.
 * 
 * history:
 *  - LFS and code navigation
 *  - read Jquery paper, Prolog language for code query (and later Semmle)
 *  - cmf --prolog for PHP
 *  - codequery, generalization for Ocaml
 *  - realization of how Prolog interactive bdd is so much better than using
 *    Ocaml and Berkeley DB to answer simple questions
 *  - idea of using Prolog for more analysis, read bddbddb paper, DOOP, etc
 *  - need of "flowing" while reading plan9 kernel code:
 *    Where this indirect interrupt function is actually called? What
 *    can be the value in this field? etc
 * 
 *
 * related work:
 * - Andersen, steengaard, manuvir das, etc
 * - DOOP, bddbddb
 * - http://pag-www.gtisc.gatech.edu/chord/user_guide/datalog.html
 * - http://blog.jetbrains.com/idea/2009/08/analyzing-dataflow-with-intellij-idea/
 *)
open Common

open Ast_minic

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type fact = string

type env = {
  scope: string; (* qualifier, usually the current function *)

  (* for constant and globals and functions *)
  globals: string list;
  structs: (string * struct_def) list;

  locals: (string * type_) list;

  facts: fact list ref;
}

let add fact env =
  Common.push fact env.facts

let _qualify _env _s = 
  raise Todo

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s name =
  failwith (spf "ERROR: %s, at %s" s (Parse_info.string_of_info (snd name)))

let var_of_global name =
  spf "'%s'" (fst name)

let var_of_local env name =
  spf "'%s__%s'" env.scope (fst name)

(* heap location, abstract memory location, heap abstraction, etc *)
let heap_of_cst _env name =
  spf "'_line%d_'" (Parse_info.line_of_info (snd name))

let var_of_name env var_or_name =
  let s = fst var_or_name in
  match Common.find_opt (fun (x, _) -> x =$= s) env.locals with
  | None ->
    if not (List.mem s env.globals)
    then error (spf "unknown entity: %s" s) var_or_name;
    var_of_global var_or_name
  | Some _t ->
    var_of_local env var_or_name

(* the variable name is also its heap abstract location as in C
 * you can get the address of any local variables.
 *)
let heap_of_name env var_or_name =
  var_of_name env var_or_name

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
let rec program env xs = 
  match xs with
  | [] -> ()
  | x::xs ->
    (match x with
    | StructDef def -> 
      let env = { env with structs = (fst def.s_name, def)::env.structs } in
      program env xs
    | FuncDef def -> 
      let env = { env with globals = (fst def.f_name)::env.globals } in
      func_def env def;
      program env xs
    | Global var ->
      let env = { env with globals = (fst var.v_name)::env.globals } in
      let name = var.v_name in
      add (spf "point_to(%s, %s)" (var_of_global name) (heap_of_cst env name)) env;
      program env xs
        
    | Constant name ->
      let env = { env with globals = (fst name)::env.globals } in
      add (spf "point_to(%s, %s)" (var_of_global name) (heap_of_cst env name)) env;
      program env xs
    )

and func_def env def =
  let env = { env with 
    scope = spf "%s" (fst def.f_name);
    locals = [];
  } in
  (* todo: parameters *)
  stmts env def.f_body

and stmts env xs =
  match xs with
  | [] -> ()
  | x::xs ->
    (match x with
    | Local var -> 
      let name = var.v_name in
      let env = { env with locals = (fst name, var.v_type)::env.locals } in
      (match var.v_type with
      | TBase _ -> 
        add (spf "point_to(%s, %s)" (var_of_local env name) (heap_of_cst env name)) env
      (* could add a point_to(%s, '_null_') for pointers *)
      | _ -> ()
      );
      stmts env xs
    | _ -> stmt env x; stmts env xs
    )

and stmt env = function
  (* should be handled in stmts *)
  | Local _ -> raise Impossible
  | Instr i -> instr env i
  | If (_var, stthen, stelse) ->
      stmts env stthen;
      stmts env stelse;
  | While (_var, st) ->
      stmts env st
  | Return _var ->
    (* TODO *)
    ()

and instr env = function
  | AssignAddress (var, name) ->
    add (spf "assign_address(%s, %s)" (var_of_name env var) (heap_of_name env name)) env
  | AssignDeref (var, var2) ->
    add (spf "assign_deref(%s, %s)" (var_of_name env var) (var_of_name env var2)) env
  | Assign (var, e) ->
    (match e with
    | Id name -> 
      add (spf "assign(%s, %s)" (var_of_name env var) (var_of_name env name)) env
    | _ -> ()
    )
      
  | _ -> ()

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
  List.rev !(env.facts)
