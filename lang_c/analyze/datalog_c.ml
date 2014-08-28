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
module D = Datalog_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Generating dataflow-related datalog facts for C.
 * 
 * See pfff/mini/datalog_minic.ml for more comments, history, and notes.
 * Lots of code in this file is copy pasted from datalog_minic.ml
 * (but now actually improved, e.g. with the notion of lvalue/rvalue).
 * 
 * todo:
 *  - could also add the AST of macros in the environment to
 *    expand sometimes
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type fact = Datalog_code.fact

type env = {
  scope: string; (* qualifier, usually the current function *)

  globals: Graph_code.graph;
  (* because of the trick we use in graph_code_c for e.g. renaming
   * static functions to avoid name collisions.
   * You need to use this function each time you think
   * a name refers to a global entity.
   *)
  globals_renames: Ast_c.name -> Ast_c.name;
  (* have option type for macro parameters ... could have a TAny also.
   * need a ref because instrs_of_expr will add new local variables.
   *)
  locals: (string * type_ option) list ref;

  facts: fact list ref;
}

(*****************************************************************************)
(* CIL-expr *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)

(* for functions, constants, fields, builtins, types *)
type name = string wrap
(* for globals, locals, parameters *)
type var = name

(* ------------------------------------------------------------------------- *)
(* Lvalue *)
(* ------------------------------------------------------------------------- *)
(* Used to be inlined in rvalue which was then called expr, but cleaner
 * to separate rvalue and lvalue. Note that 'Call' is not there, it's
 * not an lvalue (you can not do 'foo() = x' in C).
 *)
type lvalue = 
  | Id of name (* actually a var or name *)
  | ObjField of var * name (* x->fld *)
  | ArrayAccess of var * var (* x[y] *)
  (* hmm mv? *)
  | DeRef of var (* *x *)

(* ------------------------------------------------------------------------- *)
(* Rvalue *)
(* ------------------------------------------------------------------------- *)
(* see ast_minic.ml for more comments about this CIL-like AST *)
type rvalue =
  | Int of string wrap
  | Float of string wrap 
  | String of string wrap (* string or char *)

  | StaticCall of name * var list (* foo(...) *)
  | DynamicCall of (*Deref*) var * var list (* ( *f)(...) *)
  | BuiltinCall of name * var list (* e.g. v + 1 *)

  (* could be a lvalue, but weird to do (malloc(...)[x] = ...) *)
  | Alloc of type_ (* malloc(sizeof(type)) *)
  | AllocArray of var * type_ (* malloc(n*sizeof(type)) *)

  | Lv of lvalue

(* ------------------------------------------------------------------------- *)
(* Stmt *)
(* ------------------------------------------------------------------------- *)

type instr =
  | Assign of var * rvalue (* x = e *)
  | AssignAddress of var * lvalue (* except Deref (no sense to do &*x) *)
  | AssignLvalue of lvalue * var (* Except Id, done by Assign *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let debug any =
  let v = Meta_ast_c.vof_any any in
  let s = Ocaml.string_of_v v in
  let ii = Lib_parsing_c.ii_of_any any in
  pr2 (spf "PB: %s" (Parse_info.string_of_info (List.hd ii)));
  pr2 s

let line_of tok = 
  Parse_info.line_of_info tok


let var_of_global env name =
  let name = env.globals_renames name in
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
  match Common.find_opt (fun (x, _) -> x =$= s) !(env.locals) with
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
  spf "'_in_%s_line_%d_col_%d'" 
    env.scope 
    (line_of (snd name))
    (Parse_info.col_of_info (snd name))

(* TODO: need to look for type of v in env to actually qualify ... *)
let fully_qualified_field _env _v fldname =
  let fld = fst fldname in
  spf "'_fld__%s'" fld

(* TODO: need to use _struct at some point *)
let fully_qualified_field_of_struct _struc fld =
  spf "'_fld__%s'" fld


let tok_of_type t =
  List.hd (Lib_parsing_c.ii_of_any (A.Type t))

let tokwrap_of_expr e =
  (), List.hd (Lib_parsing_c.ii_of_any (A.Expr e))

let var_of_instr instr =
  match instr with
  | Assign (v, _) | AssignAddress (v, _) | AssignLvalue (_, v) -> v

exception NotSimpleExpr

let string_of_op _str =
  "_op_todo"

(*****************************************************************************)
(* Normalize *)
(*****************************************************************************)

(* The goal is to transform constructs from Ast_c to simpler constructs .
 * Here are the simplifications done:
 * - linearization of expression
 * - sugar removal for postfix/infix increments
 * - ???
 *)

(* could also use gensym *)
let counter = ref 0
(* note that we still use var_lof_name to generate the extra scope info
 * so no need to add it there
 *)
let fresh_var env (_, tok) = 
  incr counter;
  let s = spf "_v_%d" (* env.scope *) !counter in
  (* todo type! *)
  env.locals := (s, None)::!(env.locals);
  s, tok

let instrs_of_expr env e =

  let instrs = ref [] in
  (* let _new_locals = ref [] with their types? ... *)

  let rec instr_of_expr e =
  match e with
  | A.Int _ | A.Float _ | A.String _ | A.Char _ 
  | A.Id _
  | A.Unary (_, (A2.DeRef, _)) 
  | A.Call _ | A.ArrayAccess _ | A.RecordPtAccess _
  | A.Binary _ 
  | A.Unary (_, ((A2.UnPlus|A2.UnMinus|A2.Tilde|A2.Not), _))
  | A.SizeOf _
  | A.GccConstructor _
    ->
      Assign (fresh_var env (tokwrap_of_expr e), rvalue_of_simple_expr e)

  | A.Assign (op, e1, A.ArrayInit xs) ->
    let ys = xs +> List.map (fun (idxopt, value) ->
      (* less? recompute e1 each time? should store in intermediate val? *)
      let access =
        match idxopt with
        | Some e -> A.ArrayAccess(e1, e)
        | None -> A.ArrayAccess(e1, A.Int ("0", snd op))
      in
      A.Assign(op, access, value)
    )
    in
    let seq = Common2.foldl1 (fun e rest -> Sequence(e, rest)) ys in
    instr_of_expr seq

  | A.Assign (op, e1, A.RecordInit xs) ->
    let ys = xs +> List.map (fun (name, value) ->
      (* less? recompute e1 each time? should store in intermediate val? *)
      let access = 
        A.RecordPtAccess
          (A.Unary (e1, (A2.GetRef, snd op)),
           name)
      in
      A.Assign(op, access, value)
    )
    in
    let seq = Common2.foldl1 (fun e rest -> Sequence(e, rest)) ys in
    instr_of_expr seq
      

  (* ok, an actual instr! For our analysis we don't care about op (we are
   * not even control flow sensitive anyway)
   *)
  | A.Assign (_op, e1, e2) ->
      let lv = lvalue_of_expr e1 in

      (match lv, e2 with
      | Id v, A.Unary (e, (A2.GetRef, _)) ->
          (match lvalue_of_expr e with
          (* less: what &( *x ) means? *)
          | DeRef _ ->
              debug (A.Expr e);
              raise Impossible
          | lv -> AssignAddress (v, lv)
          )
      | _ ->      
          (match lv with
          | Id name -> 
              Assign (name, 
                      try rvalue_of_simple_expr e2
                      with NotSimpleExpr -> Lv (Id (var_of_expr e2))
              )
          | lv -> AssignLvalue (lv, var_of_expr e2)
          )
      )

  | A.Unary (e, (A2.GetRef, tok)) ->
      let v = fresh_var env ((), tok) in
      let lv = lvalue_of_expr e in
      (match lv with
      | DeRef _ -> 
          debug (A.Expr e);
          raise Impossible
      | lv -> AssignAddress (v, lv)
      )
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

  (* Could try to expand to a '_builtin_cond(e1, e2, e3)' but 
   * what would be the type of this function? bool -> T -> T -> T ...
   * need polymorphic type. So for now just expand to
   * 'v1 = e1; v2 = e2; v2 = e3;'
   *)
  | A.CondExpr (e1, e2, e3) ->
    let i1 = instr_of_expr e1 in
    Common.push i1 instrs;
    let tokwrap = tokwrap_of_expr e2 in
    let v = fresh_var env tokwrap in
    let tok = snd tokwrap in
    let i2 = 
      instr_of_expr (A.Assign ((Ast_cpp.SimpleAssign, tok), A.Id v, e2)) in
    Common.push i2 instrs;
    instr_of_expr (A.Assign ((Ast_cpp.SimpleAssign, tok), A.Id v, e3));

  (* like GccConstructor can be outside Assign context when in macro *)
  | A.ArrayInit _ | A.RecordInit _ ->
      debug (A.Expr e);
      let tokwrap = tokwrap_of_expr e in
      let v = fresh_var env tokwrap in
      let tok = snd tokwrap in
      instr_of_expr (A.Assign ((Ast_cpp.SimpleAssign, tok), A.Id v, e))

  and rvalue_of_simple_expr e =
  match e with
  | A.Int x -> Int x
  | A.Float x -> Float x
  | A.String x -> String x
  | A.Char x -> String x
  (* could be lots of things, global, local, param, constant, function! *)
  | A.Id name -> Lv (Id name)
  | A.Unary (e, (A2.DeRef, _)) -> Lv (DeRef (var_of_expr e))
  | A.Call (A.Id ("malloc", tok), es) ->
      (match es with
      | [SizeOf(Right(t))] -> Alloc (t)
      | [Binary(e, (Ast_cpp.Arith(Ast_cpp.Mul), _), SizeOf(Right(t)))] ->
          let v = var_of_expr e in
          AllocArray(v,t)
      | [SizeOf(Left(_e))] ->
          (* todo: need potentially to resolve the type of e *)
          (* debug (Expr e); *)
          Alloc (A.TBase ("_unknown_", tok))

      | _ -> 
          debug (Expr e);
          Alloc (A.TBase ("_unknown_", tok))
      )

  | A.Call (e, es) ->
      let vs = List.map var_of_expr es in
      (match e with
      (* todo: there is actually sugar when name is actually a global
       * and not a function
       *)
      | A.Id name -> 
          StaticCall (name, vs)
      (* ( *f)(...) *)
      | A.Unary (e, (A2.DeRef, _)) ->
          DynamicCall (var_of_expr e, vs)
      (* x->f(...) is actually sugar for ( *  x->f)(...) *)
      | A.RecordPtAccess (_, _) 
      (* x[y](...) is also sugar for ( * x[y](...) *)
      | A.ArrayAccess (_, _) 
        ->
          DynamicCall (var_of_expr e, vs)
      | _ -> 
          debug (Expr e);
          raise Todo
      )
  | A.Binary (e1, (_op, tok), e2) ->
      let vs = List.map var_of_expr [e1; e2] in
      BuiltinCall (("_builtin_" ^ (string_of_op tok), tok), vs)
  | A.Unary (e, ((A2.UnPlus|A2.UnMinus|A2.Tilde|A2.Not), tok)) ->
      let vs = [var_of_expr e] in
      BuiltinCall (("_builtin_" ^ (string_of_op tok), tok), vs)

  | A.ArrayAccess (e1, e2) ->
      let v1 = var_of_expr e1 in
      let v2 = var_of_expr e2 in
      Lv (ArrayAccess (v1, v2))
  | A.RecordPtAccess (e, name) ->
      let v = var_of_expr e in
      Lv (ObjField (v, name))

  | A.SizeOf (Left e) ->
      let instr = instr_of_expr e in
      Common.push instr instrs;
      Int ("0_sizeof", tokwrap_of_expr e +> snd)
  | A.SizeOf (Right t) ->
      Int ("0_sizeof", tok_of_type t)

  (* can be in macro context, e.g. #define SEG (struct x) { ... } *)
  | A.GccConstructor (t, _eTODO) -> Alloc t

  | _ -> 
    raise NotSimpleExpr

  and var_of_expr e =
  match e with
  | A.Id name -> name
  | _ -> 
      let instr = instr_of_expr e in
      Common.push instr instrs;
      var_of_instr instr

  and lvalue_of_expr e =
    try 
      (match rvalue_of_simple_expr e with
      | Lv x -> x
      | _ -> Id (var_of_expr e)
      )
    with NotSimpleExpr -> 
      Id (var_of_expr e)

  in
  let i = instr_of_expr e in
  List.rev (i::!instrs)

(*****************************************************************************)
(* Fact generation *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Defs *)
(* ------------------------------------------------------------------------- *)
let facts_of_def env def =
  match def with
  | StructDef def -> 
      def.s_flds +> Common.map_filter (fun fld ->
        match fld.fld_name with
        (* todo: kencc ext field! *)
        | None -> None
        | Some name ->
          (match fld.fld_type with
          | TBase _ -> 
             Some (D.PointTo ((fully_qualified_field_of_struct 
                               (fst def.s_name) (fst name)),
                            (heap_of_cst env name)))
          (* could add a point_to(%s, '_null_') for pointers *)
          | _ -> None
          )
      )
  | Define (name, _body) ->
      [D.PointTo (var_of_global env name, heap_of_cst env name)]
  | EnumDef def ->
      let (_name, xs) = def in
      xs +> List.map (fun (name, _eopt) ->
        D.PointTo (var_of_global env name, heap_of_cst env name)
      )
  | Macro _ ->
      (* todo? *)
      []
  | FuncDef def ->
      let (_ret, params) = def.f_type in
      params +> Common.index_list_1 +> Common.map_filter (fun (p, i) ->
        match p.p_name with
        | None -> None
        | Some name ->
            Some (D.Parameter (var_of_global env def.f_name,
                               i, 
                               var_of_local env name))
      ) @
     (* less: could skip when return void *)
       (let name = env.globals_renames def.f_name in
       [D.Return (var_of_global env def.f_name, spf "'ret_%s'" (fst name))]
       )
  | Global var ->      
      let name = var.v_name in
      (match var.v_type with
      | TBase _ -> 
        [D.PointTo (var_of_global env name, heap_of_cst env name)]
      (* could add a point_to(%s, '_null_') for pointers *)
      | _ -> []
      )

  | Include _ | TypeDef _ | Prototype _ -> raise Impossible


(* ------------------------------------------------------------------------- *)
(* Instr *)
(* ------------------------------------------------------------------------- *)

let facts_of_instr env = function
  | Assign (var, e) ->
      let dest = var_of_name env var in
      (match e with
      | Int x -> [D.PointTo(dest, spf "'_cst__%s'" (fst x))]
      | Float x -> [D.PointTo(dest, spf "'_float__line%d'" (line_of(snd x)))]
      | String x -> [D.PointTo(dest, spf "'_str__line%d'" (line_of (snd x)))]

      (* like in miniC *)
      | StaticCall (("printf", _), _args) -> []

      | StaticCall (name, args) 
      | DynamicCall (name, args) 
      | BuiltinCall(name, args)  ->
          let invoke = invoke_loc_of_name env name in
          args +> Common.index_list_1 +> List.map (fun (v, i) ->
            D.Argument(invoke, i, var_of_name env v)
          ) @
          [D.ReturnValue (invoke, dest)] @
          (match e with
          | StaticCall _ | BuiltinCall _ ->
              [D.CallDirect(invoke, var_of_global env name)]
          | DynamicCall _ ->
              [D.CallIndirect(invoke, var_of_name env name)]
          | _ -> raise Impossible
          )

      (* TODO: could be enum or constant! lookup g *)
      | Lv (Id name) -> 
          [D.Assign (dest, var_of_name env name)]
      | Lv (DeRef var2) ->
          [D.AssignContent(dest, var_of_name env var2)]
      | Lv (ObjField (var2, fld)) ->
          [D.AssignLoadField (dest, var_of_name env var2, 
                            fully_qualified_field env var2 fld)]
      | Lv (ArrayAccess (var2, _vidx)) -> 
         (* less: could also add info that vidx must be an int *)
         [D.AssignArrayElt(dest, var_of_name env var2)]

      | Alloc t -> 
          let tok = tok_of_type t in
          let pt = spf "'_malloc_in_%s_line_%d_'" env.scope  (line_of tok) in
          [D.PointTo(dest, pt)]
      | AllocArray (_v, t) ->
          let tok = tok_of_type t in
          let pt =  spf "'_array_in_%s_line_%d_'" env.scope (line_of tok) in
          let pt2 = spf "'_array_elt_in_%s_line_%d_'" env.scope (line_of tok) in
          [D.PointTo(dest, pt);
           D.ArrayPointTo(pt, pt2);
          ]

      )

  | AssignLvalue (ArrayAccess (varr, _vidx), vval) ->
      (* less: could also add info that vidx must be an int *)
      [D.AssignArrayDeref( var_of_name env varr, var_of_name env vval)]
  | AssignLvalue (ObjField (var, fld), var2) -> 
      [D.AssignStoreField (var_of_name env var,
                           fully_qualified_field env var2 fld,
                           var_of_name env var2)]
  | AssignLvalue (DeRef var, var2) ->
      [D.AssignDeref( var_of_name env var, var_of_name env var2)]

  | AssignAddress (var, Id name) ->
      [D.AssignAddress (var_of_name env var, heap_of_name env name)]
  | AssignAddress (var, ArrayAccess(varray, _vidx)) ->
      (* less: could also add info that vidx must be an int *)
      [D.AssignArrayElementAddress(var_of_name env var, var_of_name env varray)]
  | AssignAddress (v, ObjField (vobj, fld)) ->
      [D.AssignFieldAddress (var_of_name env v, var_of_name env vobj,
                             fully_qualified_field env vobj fld)]


  | AssignAddress (_var, DeRef _) ->
      raise Impossible
  | AssignLvalue (Id _name, _var) ->
      raise Impossible


let return_fact env instr =
  let var = var_of_instr instr in
  D.Assign(spf "'ret_%s'" env.scope, var_of_name env var)

