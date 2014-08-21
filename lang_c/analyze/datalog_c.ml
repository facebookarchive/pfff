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
module A = Ast_c
module A2 = Ast_cpp
module PI = Parse_info

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
(* see ast_minic.ml for more comments about this CIL-like AST *)
type expr =
  | Int of string wrap
  | Float of string wrap 
  | String of string wrap (* string or char *)
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

(* todo? have a lvalue type? so ObjField, ArrayAccess, Id, DeRef
 * are lvalues, which then generate different form of instr below
 *)
type instr =
  | Assign of var * expr (* x = e *)
  | AssignField of var * name * var (* x->f = v *)
  | AssignArray of var * var * var (* x[y] = v *)

  | AssignAddress of var * name (* x = &v *) (* of global, local, param, func *)
  | AssignFieldAddress of var * var * name (* x = &v->field *)
  | AssignIndexAddress of var * var * var (* x = &v[y] *)

  | AssignDeref of var * var (* *x = v *)


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

let debug any =
  let v = Meta_ast_c.vof_any any in
  let s = Ocaml.string_of_v v in
  pr2 s

let line_of tok = 
  Parse_info.line_of_info tok

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
    (fst name) (line_of (snd name))

let invoke_loc_of_name env name =
  spf "'_in_%s_line_%d_'" env.scope (line_of (snd name))

(* TODO: need to look for type of v in env to actually qualify ... *)
let fully_qualified_field _env _v fldname =
  let fld = fst fldname in
  spf "'_fld__%s'" fld

(* TODO: need to use _struct at some point *)
let fully_qualified_field_of_struct _struc fld =
  spf "'_fld__%s'" fld


let tok_of_type _t =
  raise Todo

let tokwrap_of_expr _e =
  raise Todo

let var_of_instr instr =
  match instr with
  | Assign (v, _) | AssignAddress (v, _) | AssignDeref (_, v) 
  | AssignField (_, _, v) | AssignArray (_, _, v)
  | AssignFieldAddress (v, _, _) | AssignIndexAddress (v, _, _)
    -> v

(*****************************************************************************)
(* Normalize *)
(*****************************************************************************)

(* The goal is to transform constructs from Ast_c to simpler constructs .
 * Here are the simplifications done:
 * - linearization of expression
 * - sugar removal for postfix/infix increments
 * - ???
 *)

(* use gensym? *)
let fresh_var _xwrap = 
  raise Todo

let instrs_of_expr e =

  let instrs = ref [] in
  (* let _new_locals = ref [] with their types? ... *)

  let rec instr_of_expr e =
  match e with
  | A.Int _ | A.Float _ | A.String _ | A.Char _ 
  | A.Id _
  | A.Unary (_, (A2.DeRef, _)) 
  | A.Call _ | A.ArrayAccess _ | A.RecordAccess _
  | A.Binary _ 
  | A.Unary (_, ((A2.UnPlus|A2.UnMinus|A2.Tilde|A2.Not), _))
    ->
      Assign (fresh_var (tokwrap_of_expr e), expr_of_simple_expr e)

  (* ok, an actual instr! *)
  | A.Assign (_op, _e1, _e2) ->
      debug (A.Expr e);
      raise Todo
  | A.Unary (_, (A2.GetRef, _)) ->
      debug (A.Expr e);
      raise Todo
  | A.Unary (_, ((A2.GetRefLabel, _))) ->
      (* ast_c_build should forbid that gccext *)
      debug (A.Expr e);
      raise Impossible


  | A.Sequence (e1, e2) ->
      let i1 = instr_of_expr e1 in
      Common.push i1 instrs;
      instr_of_expr e2
  | A.Cast (_tTODO, e) ->
      instr_of_expr e
  (* for pointer analysis we don't care to respect the exact semantic, we
   * are not even control flow sensitive
   *)
  | A.Postfix (e, _op) | A.Infix (e, _op) ->
      instr_of_expr e

  | A.CondExpr (_, _, _)
  | A.SizeOf _
  | A.ArrayInit _ | A.RecordInit _
  | A.GccConstructor (_, _)
      -> 
    debug (A.Expr e);
    raise Todo


  and expr_of_simple_expr e =
  match e with
  | A.Int x -> Int x
  | A.Float x -> Float x
  | A.String x -> String x
  | A.Char x -> String x
  (* could be lots of things, global, local, param, constant, function! *)
  | A.Id name -> Id name
  | A.Unary (e, (A2.DeRef, _)) -> DeRef (var_of_expr e)
  | A.Call (e, es) ->
      let vs = List.map var_of_expr es in
      (match e with
      | A.Id name -> StaticCall (name, vs)
      | _ -> DynamicCall (var_of_expr e, vs)
      )
  | A.Binary (e1, (_op, tok), e2) ->
      let vs = List.map var_of_expr [e1; e2] in
      BuiltinCall ((PI.str_of_info tok, tok), vs)
  | A.Unary (e, ((A2.UnPlus|A2.UnMinus|A2.Tilde|A2.Not), tok)) ->
      let vs = [var_of_expr e] in
      BuiltinCall ((PI.str_of_info tok, tok), vs)

  | A.ArrayAccess (e1, e2) ->
      let v1 = var_of_expr e1 in
      let v2 = var_of_expr e2 in
      ArrayAccess (v1, v2)
  | A.RecordAccess (e, name) ->
      let v = var_of_expr e in
      ObjField (v, name)
  | _ -> 
    (* dispatched correctly in instr_of_expr?? *)
    raise Impossible

  and var_of_expr e =
  match e with
  (* todo? make sure it's actually a local/param? *)
  | A.Id name -> name
  | _ -> 
    let instr = instr_of_expr e in
    var_of_instr instr
      

  in
  let i = instr_of_expr e in
  List.rev (i::!instrs)

(*****************************************************************************)
(* Fact generation *)
(*****************************************************************************)

let facts_of_instr env = function
  | AssignAddress (var, name) ->
      [spf "assign_address(%s, %s)"(var_of_name env var)(heap_of_name env name)]
  | AssignDeref (var, var2) ->
      [(spf "assign_deref(%s, %s)" (var_of_name env var)(var_of_name env var2))]
  | Assign (var, e) ->
      let dest = var_of_name env var in
      (match e with
      | Int x -> [spf "point_to(%s, '_cst__%s')" dest (fst x)]
      | Float x -> [spf "point_to(%s, '_float__line%d')" dest (line_of(snd x))]
      | String x -> [spf "point_to(%s, '_str__line%d')" dest (line_of (snd x))]
      (* TODO: could be enum or constant! lookup g *)
      | Id name -> 
          [spf "assign(%s, %s)" dest (var_of_name env name)]

      | StaticCall (name, args) 
      | DynamicCall (name, args) 
      | BuiltinCall(name, args)  ->
          let invoke = invoke_loc_of_name env name in
          args +> Common.index_list_1 +> List.map (fun (v, i) ->
            spf "argument(%s, %d, %s)" invoke i (var_of_name env v)
          ) @
          [spf "call_ret(%s, %s)" invoke dest] @
          (match e with
          | StaticCall _ | BuiltinCall _ ->
              [spf "call_direct(%s, %s)"  invoke (var_of_global env name)]
          | DynamicCall _ ->
              [spf "call_indirect(%s, %s)" invoke (var_of_name env name)]
          | _ -> raise Impossible
          )

      | DeRef var2 ->
          [spf "assign_content(%s, %s)" dest (var_of_name env var2)]

      | Alloc t -> 
          let tok = tok_of_type t in
          let pt = spf "'_malloc_in_%s_line_%d_'" env.scope  (line_of tok) in
          [spf "point_to(%s, %s)" dest pt]
      | AllocArray (_v, t) ->
          let tok = tok_of_type t in
          let pt =  spf "'_array_in_%s_line_%d_'" env.scope (line_of tok) in
          let pt2 = spf "'_array_elt_in_%s_line_%d_'" env.scope (line_of tok) in
          [spf "point_to(%s, %s)" dest pt;
           spf "array_point_to(%s, %s)" pt pt2;
          ]

      | ObjField (var2, fld) ->
          [spf "assign_load_field(%s, %s, %s)" 
              dest (var_of_name env var2) (fully_qualified_field env var2 fld)]
      | ArrayAccess (var2, _vidx) -> 
         (* less: could also add info that vidx must be an int *)
         [spf "assign_array_elt(%s, %s)" dest (var_of_name env var2)]
      )

  | AssignArray (varr, _vidx, vval) ->
      (* less: could also add info that vidx must be an int *)
      [spf "assign_array_deref(%s, %s)" 
          (var_of_name env varr) (var_of_name env vval)]
  | AssignIndexAddress (var, varray, _vidx) ->
      (* less: could also add info that vidx must be an int *)
      [spf "assign_array_element_address(%s, %s)" 
             (var_of_name env var) 
             (var_of_name env varray)]
      
  | AssignField (var, fld, var2) -> 
      [spf "assign_store_field(%s, %s, %s)" 
             (var_of_name env var)
             (fully_qualified_field env var2 fld)
             (var_of_name env var2)]

  | AssignFieldAddress (v, vobj, fld) ->
      [spf "assign_field_address(%s, %s, %s)"
             (var_of_name env v)
             (var_of_name env vobj)
             (fully_qualified_field env vobj fld)]
