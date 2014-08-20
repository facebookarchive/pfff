(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

open Ast_c
module Ast = Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Generating dataflow-related datalog facts for C.
 * 
 * See pfff/mini/datalog_minic.ml for more comments, history, and notes.
 * Lots of code in this file is copy pasted from datalog_minic.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Facts *)
(* ------------------------------------------------------------------------- *)

type fact = string
(* todo: Datalog_code.fact *)

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)

(* for functions, constants, fields, builtins, types *)
type name = string wrap
(* for globals, locals, parameters *)
type var = name

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
  | Int of string wrap
  | String of string wrap
  | Id of name (* can be a global, local, parameter, constant, functions *)

  | DeRef of var (*  *x *)

  | Alloc of type_ (* malloc(sizeof(type)) *)
  | AllocArray of var * type_ (* malloc(n*sizeof(type)) *)
  | ObjField of var * name (* x->fld *)
  | ArrayAccess of var * var (* x[y] *)
  | StaticCall of name * var list (* foo(...) *)
  | DynamicCall of var * var list (* ( *f)(...) *)
  | BuiltinCall of name * var list (* e.g. v + 1 *)

(* ------------------------------------------------------------------------- *)
(* Stmt *)
(* ------------------------------------------------------------------------- *)

type instr =
  | Assign of var * expr (* x = e *)
  | AssignAddress of var * name (* x = &v *) (* of global, local, param, func *)
  | AssignDeref of var * var (* *x = v *)

  | AssignField of var * name * var (* x->f = v *)
  | AssignArray of var * var * var (* x[y] = v *)
  | AssignFieldAddress of var * var * name (* x = &v->field *)
  | AssignIndexAddress of var * var * var (* x = &v[y] *)

(* ------------------------------------------------------------------------- *)
(* Env *)
(* ------------------------------------------------------------------------- *)

type env = {
  scope: string; (* qualifier, usually the current function *)

  globals: Graph_code.graph;
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


let var_of_global _env name =
  let s = fst name in
(*
  if Common.find_opt (fun (x,_) -> x =$= s) env.globals = None
  then error (spf "unknown global: %s" s) name;
*)
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

let invoke_loc_of_name env name =
  spf "'_in_%s_line_%d_'" env.scope (Parse_info.line_of_info (snd name))

(* TODO: need to look for type of v in env to actually qualify ... *)
let fully_qualified_field _env _v fldname =
  let fld = fst fldname in
  spf "'_fld__%s'" fld

(* TODO: need to use _struct at some point *)
let fully_qualified_field_of_struct _struc fld =
  spf "'_fld__%s'" fld


let tok_of_type _t =
  raise Todo

(*****************************************************************************)
(* Normalize *)
(*****************************************************************************)


(*****************************************************************************)
(* Fact generation *)
(*****************************************************************************)

let instr env = function
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
      (* TODO: could be enum or constant! lookup g *)
      | Id name -> 
          add (spf "assign(%s, %s)" dest (var_of_name env name)) env

      | StaticCall (name, args) 
      | DynamicCall (name, args) 
      | BuiltinCall(name, args)  ->
          let invoke = invoke_loc_of_name env name in
          args +> Common.index_list_1 +> List.iter (fun (v, i) ->
            add (spf "argument(%s, %d, %s)" invoke i (var_of_name env v)) env
          );
          add (spf "call_ret(%s, %s)" invoke dest) env;
          (match e with
          | StaticCall _ | BuiltinCall _ ->
              add (spf "call_direct(%s, %s)" 
                     invoke (var_of_global env name)) env;
          | DynamicCall _ ->
              add (spf "call_indirect(%s, %s)" 
                     invoke (var_of_name env name)) env;
          | _ -> raise Impossible
          )

      | DeRef var2 ->
          add (spf "assign_content(%s, %s)" dest (var_of_name env var2)) env

      | Alloc t -> 
          let tok = tok_of_type t in
          let pt = 
            spf "'_malloc_in_%s_line_%d_'" env.scope 
              (Parse_info.line_of_info tok) in
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
