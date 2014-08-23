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
 * See h_program-lang/datalog_code.dl for the types of the facts generated here.
 * (todo: have a datalog_fact.ml that can then be translated to luadatalog,
 * bddbddb, logicblox, etc)
 * 
 * It's not that easy to translate the DOOP code for Java to C. We could 
 * do a C -> Java translator and think about how certain C features 
 * could be emulated in Java; this can give ideas. But right now
 * reading the seminal paper on pointer analysis for C (Andersen thesis)
 * is the best approach.
 * 
 * history:
 *  - LFS and code navigation, PofFS
 *  - read Jquery paper, Prolog language for code query (and later Semmle)
 *  - cmf --prolog for PHP for class hiearchy and then for call graph and
 *    data graph (partial)
 *  - codequery, generalization for Ocaml
 *  - saw how Prolog interactive bdd was so much better than using
 *    Ocaml and Berkeley DB to answer simple questions, to perform simple 
 *    analysis
 *  - idea of using Prolog for more analysis, read bddbddb paper, DOOP, etc
 *  - saw need for "flowing" while reading plan9 kernel code:
 *    'where this indirect interrupt function is actually called?' 'what
 *    can be the value in this field?' etc
 * 
 * notes:
 *  - heap modeling? shape types? with this different point_to, array_point_to,
 *    field_point_to it's clear we want different classifications, a better
 *    organization of the heap and the relation between memory. This is
 *    probably what shape types are all about.
 *)
open Common

open Ast_minic

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type fact = string
(* todo: Datalog_code.fact *)

type env = {
  scope: string; (* qualifier, usually the current function *)

  (* actually for constant, globals and functions *)
  globals: (string * type_) list;
  structs: (string * struct_def) list;
  locals: (string * type_) list;

  facts: fact list ref;
}

let add fact env =
  Common.push fact env.facts

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s name =
  failwith (spf "ERROR: %s, at %s" s (Parse_info.string_of_info (snd name)))


let var_of_global env name =
  let s = fst name in
  if Common.find_opt (fun (x,_) -> x =$= s) env.globals = None
  then error (spf "unknown global: %s" s) name;
  spf "'%s'" s

let var_of_local env name =
  spf "'%s__%s'" env.scope (fst name)

let var_of_name env var_or_name =
  let s = fst var_or_name in
  match Common.find_opt (fun (x, _) -> x =$= s) env.locals with
  | None -> var_of_global env var_or_name
  | Some _t -> var_of_local env var_or_name

(* the variable name is also its heap abstract location as in C
 * you can get the address of any local variables.
 *)
let heap_of_name env var_or_name =
  var_of_name env var_or_name

(* heap location, abstract memory location, heap abstraction, etc *)
let heap_of_cst _env name =
  spf "'_val_of_%s_line%d_'" 
    (fst name) (Parse_info.line_of_info (snd name))

let rec tok_of_type = function
  | TBase name -> snd name
  | TPointer t | TArray t -> tok_of_type t
  | TStructName name -> snd name
  | TFunction (ret, _) -> tok_of_type ret

let invoke_loc_of_name env name =
  spf "'_in_%s_line_%d_col_%d'" 
    env.scope 
    (Parse_info.line_of_info (snd name))
    (Parse_info.col_of_info (snd name))


(* TODO: need to look for type of v in env to actually qualify ... *)
let fully_qualified_field _env _v fldname =
  let fld = fst fldname in
  spf "'_fld__%s'" fld

(* TODO: need to use _struct at some point *)
let fully_qualified_field_of_struct _struc fld =
  spf "'_fld__%s'" fld

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
let rec program env xs = 

(* ---------------------------------------------------------------------- *)
(* Toplevel *)
(* ---------------------------------------------------------------------- *)
  match xs with
  | [] -> ()
  | x::xs ->
    (match x with
    | StructDef def -> 
      let env = { env with structs = (fst def.s_name, def)::env.structs } in

      def.s_flds +> List.iter (fun var ->
        let name = var.v_name in
        (match var.v_type with
          | TBase _ -> 
            add (spf "point_to(%s, %s)" 
                   (fully_qualified_field_of_struct (fst def.s_name) (fst name))
                   (heap_of_cst env name) )env;
          (* could add a point_to(%s, '_null_') for pointers *)
          | _ -> ()
        );
      );
      program env xs
    | FuncDef def -> 
      let newglob = (fst def.f_name, TFunction def.f_type) in
      let env = { env with globals = newglob::env.globals } in
      func_def env def;
      program env xs
    | Global var ->
      let newglob = (fst var.v_name, var.v_type) in
      let env = { env with globals = newglob::env.globals } in
      let name = var.v_name in
      (match var.v_type with
      | TBase _ -> 
        add (spf "point_to(%s, %s)" 
               (var_of_global env name) (heap_of_cst env name))env;
      (* could add a point_to(%s, '_null_') for pointers *)
      | _ -> ()
      );
      program env xs
        
    | Constant name ->
      let newglob = (fst name, TBase ("int", snd name)) in
      let env = { env with globals = newglob::env.globals } in
      add (spf "point_to(%s, %s)" 
             (var_of_global env name) (heap_of_cst env name)) env;
      program env xs
    )

and func_def env def =
  let (_ret, params) = def.f_type in
  let env = { env with 
    scope = fst def.f_name;
    locals = params +> List.map (fun p -> fst p.v_name, p.v_type);
  } in
  params +> Common.index_list_1 +> List.iter (fun (p, i) ->
    add (spf "parameter(%s, %d, %s)" 
           (var_of_global env def.f_name) i (var_of_local env p.v_name)) env;
  );
  (* less: could skip when return void *)
  add (spf "return(%s, 'ret_%s')" 
         (var_of_global env def.f_name) (fst def.f_name)) env;
  stmts env def.f_body

(* ---------------------------------------------------------------------- *)
(* Stmts *)
(* ---------------------------------------------------------------------- *)

and stmts env xs =
  match xs with
  | [] -> ()
  | x::xs ->
    (match x with
    | Local var -> 
      let name = var.v_name in
      let env = { env with locals = (fst name, var.v_type)::env.locals } in
      (* hmm not sure it adds value to add this point_to. If what you want
       * is the dependency chain, it should be inferred back from the
       * datalog fact as Whaley did in his thesis in chapter 5.
       *)
      (*
      (match var.v_type with
      | TBase _ -> 
        add (spf "point_to(%s, %s)" 
               (var_of_local env name) (heap_of_cst env name)) env
      (* could add a point_to(%s, '_null_') for pointers *)
      | _ -> ()
      );
      *)
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
  | Return var ->
      add (spf "assign('ret_%s', %s)" 
             env.scope (var_of_name env var)) env

(* ---------------------------------------------------------------------- *)
(* Instr/Expr *)
(* ---------------------------------------------------------------------- *)

(* todo: rewrite to return list of facts instead like in datalog_c.ml
 * and also rewrite to use lvalue/rvalue
 *)
and instr env = function
  | AssignAddress (var, name) ->
    add (spf "assign_address(%s, %s)" 
           (var_of_name env var) (heap_of_name env name)) env
  | AssignDeref (var, var2) ->
    add (spf "assign_deref(%s, %s)" 
           (var_of_name env var) (var_of_name env var2)) env
  | Assign (var, e) ->
    let dest = var_of_name env var in
    (match e with
    | Int x ->
      add (spf "point_to(%s, '_cst__%s')" dest (fst x)) env
    | String x ->
      add(spf "point_to(%s, '_str__line%d')" 
            dest (Parse_info.line_of_info (snd x))) env

    | Id name -> 
      add (spf "assign(%s, %s)" dest (var_of_name env name)) env

    | StaticCall (name, args) | DynamicCall (name, args) | BuiltinCall(name, args)  ->
      let invoke = invoke_loc_of_name env name in
      args +> Common.index_list_1 +> List.iter (fun (v, i) ->
        add (spf "argument(%s, %d, %s)" invoke i (var_of_name env v)) env
      );
      add (spf "call_ret(%s, %s)" invoke dest) env;
      (match e with
      | StaticCall _ | BuiltinCall _ ->
        add (spf "call_direct(%s, %s)" invoke (var_of_global env name)) env;
      | DynamicCall _ ->
        add (spf "call_indirect(%s, %s)" invoke (var_of_name env name)) env;
      | _ -> raise Impossible
      )

    | DeRef var2 ->
      add (spf "assign_content(%s, %s)" dest (var_of_name env var2)) env

    | Alloc t -> 
      let tok = tok_of_type t in
      let pt = spf "'_malloc_in_%s_line_%d_'" env.scope (Parse_info.line_of_info tok) in
      add (spf "point_to(%s, %s)" dest pt) env
    | AllocArray (_v, t) ->
      let tok = tok_of_type t in
      let pt = 
        spf "'_array_in_%s_line_%d_'" 
          env.scope (Parse_info.line_of_info tok) in
      let pt2 = 
        spf "'_array_elt_in_%s_line_%d_'" 
          env.scope (Parse_info.line_of_info tok) in
      add (spf "point_to(%s, %s)" dest pt) env;
      add (spf "array_point_to(%s, %s)" pt pt2) env;

    | ObjField (var2, fld) ->
      add (spf "assign_load_field(%s, %s, %s)" 
             dest (var_of_name env var2) (fully_qualified_field env var2 fld))
        env
    | ArrayAccess (var2, _vidx) -> 
      (* less: could also add info that vidx must be an int *)
      add (spf "assign_array_elt(%s, %s)" dest (var_of_name env var2)) env
    )

  | AssignArray (varr, _vidx, vval) ->
      (* less: could also add info that vidx must be an int *)
      add (spf "assign_array_deref(%s, %s)" 
             (var_of_name env varr) 
             (var_of_name env vval)
      ) env
  | AssignIndexAddress (var, varray, _vidx) ->
      (* less: could also add info that vidx must be an int *)
      add (spf "assign_array_element_address(%s, %s)" 
             (var_of_name env var) 
             (var_of_name env varray)
      ) env

      
  | AssignField (var, fld, var2) -> 
      add (spf "assign_store_field(%s, %s, %s)" 
             (var_of_name env var)
             (fully_qualified_field env var2 fld)
             (var_of_name env var2)) env

  | AssignFieldAddress (v, vobj, fld) ->
    add (spf "assign_field_address(%s, %s, %s)"
           (var_of_name env v)
           (var_of_name env vobj)
           (fully_qualified_field env vobj fld)) env

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
