(* Julien Verlaguet
 *
 * Copyright (C) 2011, 2012 Facebook
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
open Ast_php_simple
open Ast_php_simple_toposort
module A = Ast_php_simple

open Env_typing_php
open Typing_helpers_php

module Builtins = Builtins_typed_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module implements a bottom-up type-inference scheme for PHP.
 * Every class is first sorted in their topological order, they are then
 * typed independently (hence bottom-up).
 * The type representation is fairly standard (Cf Env_typing_php),
 * except for classes. A class is represented as an object with 
 * a special field called __obj that contains the type of the
 * instanciated object.
 *
 * Example:
 *  class A {
 *    public static function f() { }
 *    public function g() { }
 *  }
 *  is represented as
 *  Tclosed (SSet('A'), SMap(
 *    'g': function unit -> unit
 *    '__obj': SMap (
 *      'f' => function unit -> unit
 *    )
 *  )
 * 
 * This module is also (ab)used to provide autocompletion.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type error =
  | UnknownEntity of Database_code.entity_kind * string

exception Error of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Unification *)
(*****************************************************************************)

module Type = struct

  (* contain both array and objects *)
  let rec mixed has_array has_object l =
    match l with
    | [] -> has_array && has_object
    | (Tobject _ | Tclosed _) :: rl -> mixed has_array true rl
    | (Trecord _ | Tarray _) :: rl -> mixed true has_object rl
    | _ :: rl -> mixed has_array has_object rl
  let mixed l = mixed false false l

  let rec unify env t1 t2 =
    if t1 == t2 then t1 else
    match t1, t2 with
    | Tsum ([] | [Tabstr "null"]), x
    | x, Tsum ([] | [Tabstr "null"]) -> x
    | x, y when x == y -> x
    | Tvar x, Tvar y when x = y -> t1
    | Tvar n1, Tvar n2 -> unify_vars env n1 n2
    | (Tvar n as x), y | y, (Tvar n as x) ->
        let n' = fresh() in
        TEnv.set env n' y;
        unify env x (Tvar n')
    | Tsum x, Tsum y ->
        let l = unify_sum env x y in
        (match () with
        | _ when mixed l           -> Tsum []
        | _ when List.length l > 3 -> Tsum []
        | _                        -> Tsum l
        )

  and unify_vars env n1 n2 =
    let n1' = Subst.get env n1 in
    let n2' = Subst.get env n2 in
    if n1' = n2' then Tvar n1' else
    let t1 = TEnv.get env n1' in
    let t2 = TEnv.get env n2' in
    let n = fresh() in
    Subst.replace env n1 n;
    Subst.replace env n2 n;
    let t = unify env t1 t2 in
    let n = Subst.get env n in
    TEnv.set env n t;
    (Tvar n)

  and unify_ env t1 t2 =
    match t1, t2 with
    | Tsstring s1, Tsstring s2 when s1 == s2 -> t1
    | Tsstring s1, Tsstring s2 ->
        let s = SSet.union s1 s2 in
        if SSet.cardinal s > 200
        then Tabstr "string"
        else Tsstring s1
    | (Tsstring _ as t), _ | _, (Tsstring _ as t) -> t
    | Tienum s1, Tienum s2 when s1 == s2 -> t1
    | Tienum s1, Tienum s2 -> Tienum (SSet.union s1 s2)
    | Tienum _, t
    | t, Tienum _ -> unify_ env t (Tabstr "int")
    | Tsenum s1, Tsenum s2 when s1 == s2 -> t1
    | Tsenum s1, Tsenum s2 -> Tsenum (SSet.union s1 s2)
    | Tsenum _, t
    | t, Tsenum _ -> unify_ env (Tabstr "string") t
    | Tabstr "bool", t (* We want them in this order bool < int < string < html *)
    | t, Tabstr "bool" -> t
    | Tabstr "int", t
    | t, Tabstr "int" -> t
    | Tabstr "string", t
    | t, Tabstr "string" -> t
    | Tabstr _, _ -> t1
    | Trecord s1, Trecord s2 ->
        if s1 == s2 then t1 else
        Trecord (unify_map env s1 s2)
    | Trecord x, t | t, Trecord x ->
        if SMap.is_empty x
        then t
        else
          let v = SMap.fold (fun _ t acc -> unify env t acc) x any in
          let s = SMap.fold (fun x _ acc -> SSet.add x acc) x SSet.empty in
          unify_ env (Tarray (s, string, v)) t
    | Tarray (s1, t1, t2), Tarray (s2, t3, t4) ->
        let s = SSet.union s1 s2 in
        Tarray (s, unify env t1 t3, unify env t2 t4)
    | Tfun (l1, t1), Tfun (l2, t2) ->
        let l = unifyl env l1 l2 in
        let t = unify env t1 t2 in
        Tfun (l, t)
    | Tobject o, Tclosed (s, c)
    | Tclosed (s, c), Tobject o ->
        let o = unify_map env o c in
        Tclosed (s, o)
    | Tobject o1, Tobject o2 ->
        if o1 == o2 then t1 else
        Tobject (unify_map env o1 o2)
    | Tclosed (s1, c1), Tclosed (s2, c2) ->
        let c1 = SMap.fold (
          fun x t acc ->
            if SMap.mem x c2
            then SMap.add x t acc
            else acc
         ) c1 SMap.empty in
        let c2 = SMap.fold (
          fun x t acc ->
            if SMap.mem x c1
            then SMap.add x t acc
            else acc
         ) c2 SMap.empty in
        Tclosed (SSet.union s1 s2, unify_map env c1 c2)
    | _ -> assert false

  and unifyl env l1 l2 =
    match l1, l2 with
    | [], l | l, [] -> l
    | (s1, x1) :: rl1, (s2, x2) :: rl2 ->
        let s = if s1 = s2 then s1 else "" in
        (s, unify env x1 x2) :: unifyl env rl1 rl2

  and unify_map env s1 s2 =
    SMap.fold (
    fun x t acc ->
      if SMap.mem x s2
      then
        let t = unify env t (SMap.find x s2) in
        SMap.add x t acc
      else SMap.add x t acc
   ) s1 s2

  and unify_sum env l1 l2 =
    match l1, l2 with
    | [], l
    | l, [] -> l
    | x1 :: rl1, x2 :: rl2 ->
        let c1 = proj x1 in
        let c2 = proj x2 in
        let c = c1 - c2 in
        if c < 0
        then x1 :: unify_sum env rl1 l2
        else if c > 0
        then x2 :: unify_sum env l1 rl2
        else unify_ env x1 x2 :: unify_sum env rl1 rl2

end

(*****************************************************************************)
(* Collect *)
(*****************************************************************************)

module Collect = struct

  type mem = {
      tys: (t, t) Hashtbl.t;
      prims: (prim_ty, prim_ty) Hashtbl.t;
    }

  let rec ty env subst tenv mem depth t =
(*    try Hashtbl.find mem.tys t
    with Not_found ->
      let t' = *)
        match t with
        | Tvar n ->
            let n = Subst.get env n in
            if IMap.mem n !subst then Tvar (IMap.find n !subst) else
            let v = fresh() in
            subst := IMap.add n v !subst;
            let t = ty env subst tenv mem depth (TEnv.get env n) in
            tenv := IMap.add v t !tenv;
            Tvar v
        | Tsum l -> Tsum (List.map (prim_ty env subst tenv mem depth) l)
(*      in
      Hashtbl.add mem.tys t t';
      t' *)

  and prim_ty env subst tenv mem depth t =
    try Hashtbl.find mem.prims t
    with Not_found ->
      if depth > 3 then t else
      let t' =
        match t with
        | Tabstr _
        | Tienum _
        | Tsstring _
        | Tsenum _ as x -> x
        | Trecord m -> Trecord (SMap.map (ty env subst tenv mem depth) m)
        | Tarray (s, t1, t2) -> 
            Tarray (s, 
                   ty env subst tenv mem depth t1, 
                   ty env subst tenv mem depth t2)
        | Tfun (tl, t) -> Tfun (
            List.map (fun (s, x) -> s, ty env subst tenv mem depth x) tl,
            ty env subst tenv mem depth t)
        | Tobject m ->
            let m = SMap.map (ty env subst tenv mem depth) m in
            Tobject m
        | Tclosed (s, m) -> 
            Tclosed (s, SMap.map (ty env subst tenv mem depth) m)
      in
      Hashtbl.add mem.prims t t';
      t'

  let ty count env subst tenv mem t =
    incr count;
    if !count mod 100 = 0
    then (Printf.printf "Collected [%d/%d]\n" !count !(env.count); flush stdout)
    else ();
    ty env subst tenv mem 0 t

  let run env =
    Printf.printf "Collecting [Cumulated: %f]: " !(env.cumul); flush stdout;
    let t = Sys.time() in
    let subst = ref IMap.empty in
    let tenv = ref IMap.empty in
    let ty = ty (ref 0) in
    let mem = { tys = Hashtbl.create 1024; prims = Hashtbl.create 1024 } in
    let lenv = SMap.map (ty env subst tenv mem) !(env.env) in
    let genv = SMap.map (ty env subst tenv mem) !(env.genv) in
    env.tenv := !tenv;
    env.subst := !subst;
    env.genv := genv;
    env.env := lenv;
    Printf.printf "Compacting: "; flush stdout;
    Gc.compact();
    Printf.printf "DONE\n"; flush stdout;
    let t = Sys.time() -. t in
    env.cumul := t +. !(env.cumul);
    env.collect_count := 0;
    Printf.printf "%f\n" t; flush stdout

  let collect env =
    incr env.collect_count;
    if !(env.collect_count) >= 100000
    then run env
    else ()
end

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program env =
  Printf.printf "Topological sort:  "; flush stdout;
  let l = TopoSort.sort env.graph in
  Printf.printf "DONE\n"; flush stdout;
  env.total := List.length l;
  List.iter (type_def env) l;
(*  Classes.iter (class_def env);
  Functions.iter (func_def env); *)
  if env.debug then Print2.penv env;
  let oc = open_out "typing_env.bin" in
  Printf.printf "Saving environment (typing_env.bin): "; flush stdout;
  Collect.run env;
  GEnv.save env oc;
  close_out oc;
  Printf.printf "DONE\n"

and type_def env x =
  if Classes.mem env x && not (GEnv.mem_class env x)
  then (class_id env x);
  if Functions.mem env x && not (GEnv.mem_fun env x)
  then (func_id env x)
  else ()

and decls env stl =
  List.iter (function
  | ClassDef cd ->
      Graph.class_def env.graph cd;
      Classes.add env (A.unwrap cd.c_name) cd
  | FuncDef fd ->
      Graph.func_def env.graph fd;
      Functions.add env (A.unwrap fd.f_name) fd
  | ConstantDef _ ->
      raise Common.Todo
  | _ -> ()
  ) stl

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmtl env l =
  List.iter (stmt env) l

and stmt env = function
  | Expr e -> iexpr env e
  | Block stl -> stmtl env stl
  | If (e, st1, st2) ->
      iexpr env e;
      stmt env st1;
      stmt env st2
  | While (e, stl) ->
      iexpr env e;
      stmtl env stl
  | Do (stl, e) ->
      stmtl env stl;
      iexpr env e
  | For (el1, el2, el3, stl) ->
      iexprl env el1;
      iexprl env el2;
      iexprl env el3;
      stmtl env stl
  | Switch (e, cl) ->
      let t = expr env e in
      casel env t cl
  | Foreach (e1, e2, eopt, stl) ->
      let a = expr env e1 in
      let a' =
        match eopt with
        | None -> array (Tvar (fresh()), expr env e2)
        | Some v -> array (expr env e2, expr env v)
      in
      let _ = Type.unify env a a' in
      stmtl env stl
  | Return None -> ()
  | Return (Some e) ->
      iexpr env (Assign (None, Id (wrap "$;return"), e))
  | Break eopt -> expr_opt env eopt
  | Continue eopt -> expr_opt env eopt
  | Throw e -> iexpr env e
  | Try (stl, c, cl) ->
      stmtl env stl;
      catch env c;
      catchl env cl
  | StaticVars svarl ->
      List.iter (fun (s, e) ->
        match e with
        | None -> ()
        | Some e ->
            iexpr env (Assign (None, Id s, e))
     ) svarl
  | Global el ->
      List.iter (function
        | Id (x, tok) ->
            let gid = String.sub x 1 (String.length x -1) in
            let gl = Array_get (Id (wrap "$GLOBALS"), Some (String gid)) in
            let assign = Assign (None, Id (x, tok), gl) in
            iexpr env assign
        | e -> iexpr env e
     ) el
  | ClassDef cd ->
      class_def env cd
  | FuncDef fd ->
      func_def env fd
  | ConstantDef cd ->
      raise Common.Todo

and expr_opt env = function
  | None -> ()
  | Some x -> iexpr env x

and casel env t l = List.iter (case env t) l

and case env t = function
  | Case (e, stl) ->
      let t' = expr env e in
      let _ = Type.unify env t t' in
      stmtl env stl
  | Default stl -> stmtl env stl

and catchl env l = List.iter (catch env) l
and catch env (_, _, stl) = stmtl env stl

and exprl env l = List.map (expr env) l
and iexprl env l = ignore (exprl env l)

and iexpr env e = ignore (expr env e)

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env e =
  expr_ env false e

and expr_ env lv = function
  | Int _ -> int
  | Double _ -> float
  | String s ->
      (match () with
      | _ when env.auto_complete && has_marker env s ->
          let t = Tvar (fresh()) in
          env.show := Sauto_complete (s, t);
          t
      | _ when String.contains s '<' -> 
          thtml
      | _ -> Tsum [Tsstring (SSet.singleton s)]
      )
  | Guil el ->
      List.iter (encaps env) el;
      string
  | Id (("true" | "false"),_) -> bool
  | Id (s, tok) ->
      let is_marked = has_marker env s in


      if env.infer_types && is_marked
      then begin
        let s = get_marked_id env s in
        let t = expr env (Id (s, tok)) in
        env.show := Stype_infer t;
        t
      end
      else if env.auto_complete && is_marked
      then begin
        if s.[0] = '$'
        then
          let locals = SMap.fold (fun x _ acc -> SSet.add x acc) !(env.env) SSet.empty in
          env.show := Slocal (get_marked_id env s, locals)
        else env.show := Sglobal (get_marked_id env s);
        any
      end
      else if s.[0] = '$' || Env.mem env s
      then Env.get env s
      else if GEnv.mem_fun env s
      then GEnv.get_fun env s
      else if GEnv.mem_class env s
      then GEnv.get_class env s
      else if Classes.mem env s || Functions.mem env s
      then (type_def env s; expr env (Id (s, tok)))
      else begin
        if env.debug then
          (Printf.printf "Unknown identifier: %s\n" s; flush stdout);
        any
      end
  | This -> expr env (Id (wrap "$this"))
  | Array_get (e, None) ->
      let t1 = expr env e in
      let v = Tvar (fresh()) in
      let t2 = array (int, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Array_get (e, Some (Id (s,_))) when s.[0] <> '$' ->
      expr env (Array_get (e, Some (String s)))
  | Array_get (Id (s,_), Some (String x))
      when Hashtbl.mem Builtins.super_globals s ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = GEnv.get_global env s in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = srecord (x, v) in
      let _ = Type.unify env t1 t2 in
      Instantiate.approx env ISet.empty v
  | Array_get (e, Some (String s))->
      let marked = env.auto_complete && has_marker env s in
      let t1 = expr env e in
      if marked then (env.show := Sauto_complete (s, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = srecord (s, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Call (Id (("idx" | "edx" | "adx" | "sdx"),_), (e :: k :: r)) ->
      let e = expr env (Array_get (e, Some k)) in
      (match r with
      | [] -> e
      | x :: _ -> Type.unify env (expr env x) e
      )
  | Array_get (e, Some k) ->
      let t1 = expr env e in
      let k = expr env k in
      let v = Tvar (fresh()) in
      let t2 = array (k, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Class_get (Id (c,_), Id (x,_)) when c <> special "self" && c <> special "parent" ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = GEnv.get_class env c in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = sobject (x, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Class_get (e, Id (x,_))
  | Obj_get (e, Id (x,_)) ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = expr env e in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = sobject (x, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Class_get _
  | Obj_get _ -> any
  | Assign (None, e1, e2) ->
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      let t = Type.unify env t1 t2 in
      t
  | Assign (Some bop, e1, e2) ->
      expr env (Assign (None, e1, Binop (bop, e1, e2)))
  | ConsArray avl ->
      let t = Tvar (fresh()) in
      let t = List.fold_left (array_value env) t avl in
      t
  | Infix (_, e) -> expr env e
  | Postfix (_, e) -> expr env e
  | Binop (bop, e1, e2) ->
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      binaryOp env t1 t2 bop
  | Unop (uop, e) ->
      let _ = expr env e in
      unaryOp uop
  | Call (e, [Id ("JUJUMARKER",_)]) ->
      env.show := Sargs (expr env e);
      any
  | Call (e, el) ->
      let f = expr env e in
      let f = Instantiate.ty env ISet.empty f in
      let v = Tvar (fresh()) in
      let f' = fun_ (exprl env el) v in
      let _ = Type.unify env f f' in
      v
  | Ref e -> expr env e
  | Xhp x ->
      xml env x;
      let name = List.fold_right (fun x acc -> x^":"^acc) x.xml_tag "" in
      let t = expr env (New (Id (wrap name), [])) in
      t
  | List el ->
      let t = Tvar (fresh()) in
      let el = List.map (expr env) el in
      let t = List.fold_left (Type.unify env) t el in
      array (int, t)
  | New (Id (x,_), _) when env.auto_complete && has_marker env x ->
      env.show := Sglobal (get_marked_id env x);
      any
  | New (x, el) ->
      let v = "$;tmp"^(string_of_int (fresh())) in
      let obj = Class_get (x, Id (wrap "__obj")) in
      iexpr env (Assign (None, Id (wrap v), obj));
      iexpr env (Call (Obj_get (obj, Id (wrap "__construct")), el));
      let t = expr env (Id (wrap v)) in
      let set = match x with
        | Id (c,_) when c.[0] <> '$' -> SSet.singleton c
        | _ -> SSet.empty
      in
      Type.unify env t (Tsum [Tclosed (set, SMap.empty)])
  | InstanceOf (e1, e2) ->
      iexpr env e1;
      iexpr env e2;
      bool
  | CondExpr (e1, e2, e3) ->
      iexpr env e1;
      let e2 = expr env e2 in
      let e3 = expr env e3 in
      Type.unify env e2 e3
  | Cast (pty, e) ->
      iexpr env e;
      ptype env pty
  | Lambda _ -> (* TODO *)
      any

and encaps env e =
  let t = expr env e in
  ignore (Type.unify env t string)

and ptype env = function
  | Ast_php.BoolTy -> bool
  | Ast_php.IntTy -> int
  | Ast_php.DoubleTy -> float
  | Ast_php.StringTy -> string
  | Ast_php.ArrayTy -> Tsum [Trecord SMap.empty]
  | Ast_php.ObjectTy -> Tsum [Tobject SMap.empty]

and array_value env t = function
  | Aval e ->
      let t' = array (int, expr env e) in
      Type.unify env t t'
  | Akval (String s, e) ->
      let t' = srecord (s, (expr env e)) in
      Type.unify env t t'
  | Akval (e1, e2) ->
      let t' = array (expr env e1, expr env e2) in
      Type.unify env t t'

and binaryOp env t1 t2 = function
  | Ast_php.Arith _ ->
      let t = Type.unify env t1 t2 in
      t
  | Ast_php.Logical lop ->
      logicalOp env t1 t2 lop;
      bool
  | Ast_php.BinaryConcat ->
      Type.unify env t1 t2

and logicalOp env t1 t2 = function
  | Ast_php.Inf | Ast_php.Sup | Ast_php.InfEq | Ast_php.SupEq
  | Ast_php.Eq | Ast_php.NotEq
  | Ast_php.Identical | Ast_php.NotIdentical ->
      ignore (Type.unify env t1 t2)
  | Ast_php.AndLog | Ast_php.OrLog | Ast_php.XorLog
  | Ast_php.AndBool | Ast_php.OrBool ->
      ()

and unaryOp = function
  | Ast_php.UnPlus | Ast_php.UnMinus | Ast_php.UnTilde -> int
  | Ast_php.UnBang -> bool

and xhp env = function
  | XhpText _ -> ()
  | XhpExpr e -> ignore (expr env e)
  | XhpXml x -> xml env x

and xml env x =
  List.iter (fun (_, x) -> xhp_attr env x) x.xml_attrs;
  List.iter (xhp env) x.xml_body

and xhp_attr env = function
  | Guil el -> List.iter (encaps env) el
  | e ->
      let t = expr env e in
      ignore (Type.unify env t string)

(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)
and func_id env fname =
  if GEnv.mem_fun env fname then () else
  try func_def env (Functions.get env fname)
  with Not_found ->
    GEnv.set_fun env fname (Tvar (fresh()));
    if env.verbose then begin
    Printf.printf "Function not found: %s\n" fname; flush stdout;
    end

and func_def env fd =
  if GEnv.mem_fun env (Ast.unwrap fd.f_name) then () else
  if env.verbose then begin
    incr env.count;
  Printf.printf "Typing function(%d/%d)[%d]: %s\n" !(env.count) !(env.total) env.depth (Ast.unwrap fd.f_name); flush stdout;
  end;
  Collect.collect env;
  let env = { env with env = ref SMap.empty } in
  let pl = List.map (parameter env) fd.f_params in
  let ret = fresh() in
  let return = Tvar ret in
  let f = Tsum [Tfun (pl, return)] in
  GEnv.set_fun env (Ast.unwrap fd.f_name) f;
  List.iter (type_def env) (Graph.get_deps !(env.graph) (Ast.unwrap fd.f_name));
  Env.set env "$;return" return;
  stmtl env fd.f_body;
  make_return env ret;
  GEnv.set_fun env (Ast.unwrap fd.f_name) (Generalize.ty env ISet.empty f)

and make_return env r =
  match TEnv.get env r with
  | Tsum _ -> TEnv.set env r null
  | _ -> ()

and parameter env p =
  let pval =
    match p.p_type with
    | None -> Tvar (fresh())
    | Some (Hint x) ->
        (try get_hard_object env x
        with Not_found ->
          expr env (New (Id (wrap x), [])))
    | Some (HintArray) ->
        expr env (ConsArray [])
  in
  (match p.p_default with
  | None -> ()
  | Some e -> ignore (Type.unify env pval (expr env e))
  );
  Env.set env (Ast.unwrap p.p_name) pval;
  (Ast.unwrap p.p_name), pval

(* ---------------------------------------------------------------------- *)
(* Classes *)
(* ---------------------------------------------------------------------- *)
and class_id env x =
  if GEnv.mem_class env x then () else
  if not (Classes.mem env x) then () else
  class_def env (Classes.get env x)

and get_hard_object env c =
  let class_ = GEnv.get_class env c in
  let class_ = match class_ with Tsum [Tobject o] -> o | _ -> raise Not_found in
  SMap.find "__obj" class_

(* the type of an object is always in the field __obj of the class
   all the other fields are static methods/vars
*)
and get_object = function
  | Tsum [Tobject o] when SMap.mem "__obj" o->
      (match SMap.find "__obj" o with Tsum [Tobject o] -> o
      | _ -> SMap.empty)
  | _ -> SMap.empty

and get_class env x =
  class_id env x;
  GEnv.get_class env x

and class_def env c =
  if GEnv.mem_class env (Ast.unwrap c.c_name) then () else begin
  GEnv.set_class env (Ast.unwrap c.c_name) any;
  Collect.collect env;
  let parent, parent_name =
    match c.c_extends with
    | [x] -> get_class env x, x
    | _ -> Tvar (fresh()), "" in
  if env.verbose then begin
    incr env.count;
    Printf.printf "Typing class(%d/%d)[%d]: %s\n" !(env.count) !(env.total) env.depth (Ast.unwrap c.c_name); flush stdout;
  end;
  let env = { env with env = ref SMap.empty } in
  let class_ = match parent with Tsum [Tobject o] -> o | _ -> SMap.empty in
  let obj_parent = get_object parent in

  (* Adding traits *)
  let traits =
    List.map (fun (x, _) -> get_object (get_class env x)) c.c_traits in
  let obj_parent = List.fold_right (SMap.fold SMap.add) traits obj_parent in

  (* Declarations *)
  let is_enum = c.c_variables = [] && c.c_methods = [] in
  let ien, sen = List.fold_left (constant_enum is_enum c.c_name) (SSet.empty, SSet.empty) c.c_constants in
  let class_ = List.fold_left (constant is_enum env ien sen) class_ c.c_constants in
  let class_ = List.fold_left (class_vars true env) class_ c.c_variables in
  let class_ = List.fold_left (method_decl true env) class_ c.c_methods in

  let obj = List.fold_left (class_vars false env) obj_parent c.c_variables in
  let obj = List.fold_left (method_decl false env) obj c.c_methods in

  let this = Tsum [Tclosed (SSet.singleton (Ast.unwrap c.c_name), obj)] in
  let self = Type.unify env parent (Tsum [Tobject class_]) in

  GEnv.set_class env (Ast.unwrap c.c_name) self;
  Env.set env (special "self") self;
  Env.set env "$this" this;
  Env.set env (special "parent") (Tsum [Tobject obj_parent]);

  let class_ = List.fold_left (method_def true env) class_ c.c_methods in
  let obj = List.fold_left (method_def false env) obj c.c_methods in

  let obj = SMap.map (fun t -> Generalize.ty env ISet.empty t) obj in
  let privates = List.fold_left private_vars SSet.empty c.c_variables in
  let privates = List.fold_left private_methods privates c.c_methods in
  let obj = filter_privates privates obj in

  (* CHEATING :-) *)
  let obj = SMap.map (cheat_method env parent_name this) obj in

  let obj = Tsum [Tobject obj] in
(*  let obj = Generalize.ty env obj in
  let obj = Tlazy obj in *)
  let class_ = SMap.map (fun t -> Generalize.ty env ISet.empty t) class_ in
  let class_ = filter_privates privates class_ in
  let class_ = SMap.add "__obj" obj class_ in
  let class_ = (Tsum [Tobject class_]) in

(*  let class_ = Generalize.ty env class_ in
  make_globals env; *)
  GEnv.set_class env (Ast.unwrap c.c_name) class_
  end

and private_vars privates cv =
  if cv.cv_visibility = Private
  then SSet.add cv.cv_name privates
  else privates

and private_methods privates m =
  if m.m_visibility = Private
  then SSet.add (Ast.unwrap m.m_name) privates
  else privates

and filter_privates privates obj =
  SMap.fold (
  fun x t acc ->
    if SSet.mem x privates
    then acc
    else SMap.add x t acc
 ) obj SMap.empty

and constant_enum is_enum cname (ien, sen) (x, e) =
  match e with
  | Int _ -> SSet.add (Ast.unwrap cname^"::"^x) ien, sen
  | String _ -> ien, SSet.add (Ast.unwrap cname^"::"^x) sen
  | _ -> ien, sen

and constant is_enum env ien sen acc (x, e) =
  match e with
  | Int _ when is_enum -> SMap.add x (Tsum [Tienum ien]) acc
  | String _ when is_enum -> SMap.add x (Tsum [Tsenum sen]) acc
  | _ -> SMap.add x (expr env e) acc

and class_vars static env acc c =
  if static && not c.cv_static then acc else
  if not static && c.cv_static then acc else
  (cv_var static env) acc (c.cv_name, c.cv_value)

and cv_var static env acc (s, e) =
  let t = match e with None -> Tvar (fresh()) | Some x -> expr env x in
  let s = if static then s else String.sub s 1 (String.length s - 1) in
  SMap.add s t acc

and method_decl static env acc m =
  if m.m_static && not static then acc else
  if not m.m_static && static then acc else
  let pl = List.map (parameter env) m.m_params in
  let ret = fresh() in
  let f = afun pl (Tvar ret) in
  SMap.add (Ast.unwrap m.m_name) f acc

and method_def static env acc m =
  if m.m_static && not static then acc else
  if not m.m_static && static then acc else
  let env_cpy = !(env.env) in
  let pl = List.map (parameter env) m.m_params in
  let ret = fresh() in
  let return = Tvar ret in
  Env.set env "$;return" return;
  stmtl env m.m_body;
  make_return env ret;
  let f = afun pl (Env.get env "$;return") in
  let _ = Type.unify env (SMap.find (Ast.unwrap m.m_name) acc) f in
  env.env := env_cpy;
  SMap.add (Ast.unwrap m.m_name) f acc

and cheat_method env parent this m =
  match m with
  | Tsum [Tfun (x, Tsum [Tclosed (s, _)])] when SSet.mem parent s ->
      let v = Tvar (fresh()) in
      let v = Type.unify env v this in
      Tsum [Tfun (x, v)]
  | x -> x
