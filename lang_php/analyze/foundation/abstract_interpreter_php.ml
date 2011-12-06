(* Julien Verlaguet
 *
 * Copyright (C) 2011 Facebook
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

open Abstract_interpreter_php_helpers
open Env_interpreter_php

module Env = Env_interpreter_php

module SSet = Set.Make (String)
module SMap = Map.Make (String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Abstract interpreter for PHP, security oriented (tainting analysis).
 *
 * 'show($x)' in the PHP file helps to debug a variable.
 * Adding a noop (e.g. with ;;) shows the environment too.
 *
 * TODO: $x++ is ignored (we don't really care about int for now)
 *
 * pad's notes:
 *  - "*return*"
 *  - "*array*
 *  - "*myobj*
 *  - "*BUILD*"
 *  - "self"/"parent"
 *  - How the method lookup mechanism works? there is no lookup,
 *    instead at the moment where we build the class, we put
 *    all the methods of the parents in the new class. But then
 *    what about the use of self:: or parent:: when executing the
 *    code of a parent method?
 *  - the id() function semantic is hardcoded
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* call stack used for debugging when found an XSS hole
 * todo: could be put in the env too, next to stack and safe.
 *)
let path = ref []

(* used by unit testing when encountering the 'checkpoint()' function call *)
let _checkpoint_heap = ref
  (None: (Env_interpreter_php.heap * value SMap.t) option)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
exception LostControl
exception UnknownFunction of string
exception Really
exception Todo of string

(*****************************************************************************)
(* Callgraph generation *)
(*****************************************************************************)
let extract_paths = ref true

(* string (function name, class+method?, ??) -> string set *)
let (graph: SSet.t SMap.t ref) = ref SMap.empty

let rec add_graph sl =
  match sl with
  | [] -> ()
  | [_] -> ()
  | x :: (y :: _ as rl) ->
      let vs = try SMap.find y !graph with Not_found -> SSet.empty in
      let vs = SSet.add x vs in
      graph := SMap.add y vs !graph;
      add_graph rl

let save_path env fname =
  if !extract_paths
  then begin
    let path = !(env.file) :: (List.rev (fname :: !path)) in
(*    List.iter (Printf.printf "%s ") path;
    Printf.printf "\n"; *)
    add_graph path
  end
  else ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec get_dynamic_function env heap v =
  let heap, v = Ptr.get heap v in
  match v with
  | Vstring s ->
      (try heap, env.db.funs s
       with Not_found -> raise (UnknownFunction s)
      )
  | Vsum l -> get_function_list env heap l
  | _ -> raise LostControl

and get_function_list env heap = function
  | [] -> raise LostControl
  | Vstring s :: _ -> heap, env.db.funs s
  | _ :: rl -> get_function_list env heap rl

(*****************************************************************************)
(* Hooks (tainting functor for now) *)
(*****************************************************************************)

module Taint = struct
    let taint_mode = ref false
    let taint_expr a b c d e = failwith "taint_expr: Todo"
    let fold_slist sl =
      List.fold_left (fun acc x ->
        match x with
        | Vtaint _ as x -> x
        | _ -> acc
      ) (Vabstr Tstring) sl
    let check_danger a b c d e f = ()
    let binary_concat env heap v1 v2 path =
      Vabstr Tstring

    module GetTaint = struct
      exception Found of string list

      let rec list f l =
        match l with
        | [] -> ()
        | [x] -> f x
        | x :: rl -> f x; list f rl
            
      let rec value path ptrs x =
        match x with
        | Vany
        | Vnull -> ()
        | Vtaint s -> raise (Found [s])
        | Vabstr _ -> ()
        | Vbool _
        | Vint _ -> ()
        | Vref s ->
            let n = ISet.choose s in
            if not (ISet.mem n path)
            then
              let path = ISet.union s path in
              (try
                  value path ptrs (IMap.find n ptrs);
                with Not_found -> ()
              )
            else ()
        | Vptr n when ISet.mem n path ->
            let path = ISet.add n path in
            value path ptrs (IMap.find n ptrs);
        | Vptr _ -> ()
        | Vfloat _ -> ()
        | Vstring _ -> ()
        | Vrecord m ->
            let vl = SMap.fold (fun x y acc -> (x, y) :: acc) m [] in
            list (fun (x, v) -> value path ptrs v) vl;
        | Vobject m ->
            let vl = SMap.fold (fun x y acc -> (x, y) :: acc) m [] in
            list (fun (x, v) -> value path ptrs v) vl;
        | Varray vl ->
            list (value path ptrs) vl;
        | Vmap (v1, v2) ->
            value path ptrs v1;
            value path ptrs v2;
        | Vmethod _ -> ()
        | Vsum vl ->
            list (value path ptrs) vl
              
      let value heap v =
        try value ISet.empty heap.ptrs v; None with Found l -> Some l
          
    end

end

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program env heap program =
  path := [];
  if !extract_paths
  then (List.iter (fake_root env heap) program);
  let heap = stmtl env heap program in
  (* Env.penv print_string env heap; *)
  heap

and fake_expr env heap =
  save_excursion env heap expr

and fake_root env heap = function
  | ClassDef c -> (* Nested ones need to be defined *)
      let make_params l =
        List.map (fun p ->
          match p.p_type with
          | Some (Hint s) -> New (Id (w s), [])
          | _ -> Id (w "null")) l in
      let heap = force_class env heap (unw c.c_name) in
      List.iter (
      fun m ->
        if m.m_static
        then
          let params = make_params m.m_params in
          fake_expr env heap
            (Call (Class_get (Id c.c_name, Id m.m_name), params))
     ) c.c_methods;
      List.iter (
      fun m ->
        if not m.m_static
        then begin
          let params = make_params m.m_params in
          fake_expr env heap
              (Call (Obj_get (New (Id c.c_name, []), Id m.m_name), params))
        end
     ) c.c_methods;
| FuncDef fd -> ignore (call_fun fd env heap [])
  | _ -> ()

(* pad: todo, what if break/continue or throw ? do we still abstract evaluate
 * the rest of the code?
 *)
and stmtl env heap stl =
  List.fold_left (stmt env) heap stl

and stmt env heap x =
  stmt_ env heap x

and stmt_ env heap x =
  match x with
  (* ---------------------------------------------------------------------- *)
  (* Debugging support *)
  (* ---------------------------------------------------------------------- *)
  (* special keyword in the code to debug the abstract interpreter state *)
  | Expr (Call (Id ("show",_), [e])) ->
      let heap, v = expr env heap e in
(*      Env.debug heap v; *)
      heap
  (* ugly: another trick to debug the abstract interpreter, use double ;; *)
  | Noop ->
(*      Env.penv print_string env heap; *)
      heap
  | Expr (Call (Id ("checkpoint",_), [])) ->
      _checkpoint_heap := Some (heap, !(env.vars));
      heap

  (* ---------------------------------------------------------------------- *)
  (* Generic code *)
  (* ---------------------------------------------------------------------- *)
  | Expr e ->
      let heap, _ = expr env heap e in
      heap
  | If (c, e1, e2) ->
      let heap, _ = expr env heap c in
      let heap = NullNewVars.stmt env heap e1 in
      let heap = NullNewVars.stmt env heap e2 in
      let heap = stmt env heap e1 in
      let heap = stmt env heap e2 in
      heap
  | Block stl ->
      stmtl env heap stl
  | Return e ->
      let e = match e with None -> Id (w "NULL") | Some e -> e in
      (* the special "*return*" variable is used in call_fun() below *)
      let heap, _ = expr env heap (Assign (None, Id (w "*return*"), e)) in
      heap
  | Global idl ->
      List.fold_left (global env) heap idl
  | ClassDef c ->
    (* todo? failwith if is a nested class because it will not be in the db *)
     heap
  | FuncDef fd ->
    (* todo? failwith if is a nested func because it will not be in the db *)
      heap
  | Do (stl, e)
  | While (e, stl) ->
      let heap, _ = expr env heap e in
      let heap = stmtl env heap stl in
      heap
  | For (el1, el2, el3, stl) ->
      let heap, _ = Utils.lfold (expr env) heap el1 in
      let heap, _ = Utils.lfold (expr env) heap el2 in
      let heap, _ = Utils.lfold (expr env) heap el3 in
      stmtl env heap stl
  | Switch (e, cl) ->
      let heap, _ = expr env heap e in
      let heap = List.fold_left (case env) heap cl in
      heap
  | Foreach (a, k, vopt, stl) ->
      let heap, a = expr env heap a in
      let heap, _, k = lvalue env heap k in
      let heap, k, v =
        match vopt with
        | None ->
            let heap, kint = Ptr.new_val heap (Vabstr Tint) in
            heap, kint, k
        | Some v ->
            let heap, _, v = lvalue env heap v in
            heap, k, v
      in
      let heap, a' = Ptr.new_val heap (Vmap (k, v)) in
      let heap, a' = Ptr.get heap a' in
      let heap, a = Unify.value heap a a' in
      let heap = stmtl env heap stl in
      heap
  | Continue e
  | Break e ->
      let heap, _ = Utils.opt (expr env) heap e in
      heap
  | Throw e ->
      let heap, _ = expr env heap e in
      heap
  | Try (stl, c, cl) ->
      let heap = stmtl env heap stl in
      let heap = catch env heap c in
      let heap = List.fold_left (catch env) heap cl in
      heap
  | StaticVars sl ->
      List.fold_left (static_var env) heap sl
  | InlineHtml _ -> heap

and case env heap x =
  match x with
  | Case (e, stl) ->
      let heap, _ = expr env heap e in
      let heap = NullNewVars.stmtl env heap stl in
      let heap = stmtl env heap stl in
      heap
  | Default stl ->
      let heap = NullNewVars.stmtl env heap stl in
      let heap = stmtl env heap stl in
      heap

and catch env heap (_, _, stl) =
  stmtl env heap stl

and expr env heap x =
  if !Taint.taint_mode
  then Taint.taint_expr env heap 
    (expr_, lvalue, get_dynamic_function, call_fun, call) !path x
  else expr_ env heap x

and expr_ env heap x =
  match x with
  (* ---------------------------------------------------------------------- *)
  (* Generic code *)
  (* ---------------------------------------------------------------------- *)
  | Call (Id ("id",_), [x]) -> expr env heap x
  | String s -> heap, Vstring s
  | Int s -> heap, Vint (int_of_string s)
  | Double s -> heap, Vfloat (float_of_string s)
  | HereDoc _ -> heap, Vabstr Tstring
  | Guil el ->
      let heap, vl = Utils.lfold (encaps env) heap el in
      let heap, vl = Utils.lfold Ptr.get heap vl in
      let v = Taint.fold_slist vl in
      heap, v

  | Ref e ->
      let heap, _, x = lvalue env heap e in
      heap, x
  | ConsArray [] ->
      heap, Varray []
  | ConsArray avl ->
      let id = Id (w "*array*") in
      let heap = List.fold_left (array_value env id) heap avl in
      let heap, _, v = Var.get env heap "*array*" in
      let heap, v = Ptr.get heap v in
      Var.unset env "*array*";
      heap, v
  | Binop (bop, e1, e2) ->
      let heap, v1 = expr env heap e1 in
      let heap, v2 = expr env heap e2 in
      let heap, v1 = Ptr.get heap v1 in
      let heap, v2 = Ptr.get heap v2 in
      heap, binaryOp env heap bop v1 v2
  | Unop (uop, e) ->
      let heap, v = expr env heap e in
      heap, unaryOp uop v
  | Call (Id ("call_user_func" as fname,tok), f :: el) ->
      let heap, f = expr env heap f in
      Taint.check_danger env heap fname tok !path f;
      (try
          let heap, f = get_dynamic_function env heap f  in
          call_fun f env heap el
        with _ -> heap, Vany)
  | Call (Id (s,_) as e, el) ->
      (try
        let heap, f = get_function env heap s e in
        call_fun f env heap el
     (* pad: other? *)
      with LostControl | UnknownFunction _  ->
        save_path env s;
        let heap, vl = Utils.lfold (expr env) heap el in
        let c = ref None in
        List.iter (fun x -> 
          let x = Taint.GetTaint.value heap x in 
          if x <> None then c := x
        ) vl;
        let res =
          (match !c with
          | _ when not !Taint.taint_mode -> Vany
          | None -> Vany
          | Some x -> Vtaint (List.hd x)
          ) in
        heap, res
      ) (* Lost control *)
  | Call (e, el) ->
      let heap, v = expr env heap e in
      call env heap v el
  | Xhp x ->
      let heap = xml env heap x in
      heap, Vabstr Txhp

  | New (c, el) ->
      let c = get_class env heap c in
      let heap = lazy_class env heap c in
      (* pad: ?? *)
      let stl = [
        Expr (Assign (None, Id (w "*myobj*"),
                     Call (Class_get (Id (w c), Id (w "*BUILD*")), el)));
        Expr (Call (Obj_get (Id (w "*myobj*"), Id (w "__construct")), el));
      ] in
      let heap = stmtl env heap stl in
      let heap, _, v = Var.get env heap "*myobj*" in
      Var.unset env "*myobj*";
      heap, v
  | InstanceOf (e1, e2) ->
      let heap, _ = expr env heap e1 in
      let heap, _ = expr env heap e2 in
      heap, Vsum [Vnull; Vabstr Tbool]
  | CondExpr (e1, e2, e3) ->
      let heap, _ = expr env heap e1 in
      let heap, v1 = expr env heap e2 in
      let heap, v2 = expr env heap e3 in
      let heap, v = Unify.value heap v1 v2 in
      heap, v
  | Cast (ty, e) ->
      let heap, v = expr env heap e in
      heap, cast env heap ty v

  | List _ -> raise Really

  | Assign (None, List l, e) ->
      let n = ref 0 in
      let heap =
        List.fold_left (
          fun heap x ->
            let v = Array_get (e, Some (Int (string_of_int !n))) in
            let heap, _ = expr env heap (Assign (None, x, v)) in
            incr n;
            heap
        ) heap l in
      let heap, e = expr env heap e in
      heap, e

  | Assign (None, e1, e2) ->
      let heap, b, root = lvalue env heap e1 in
      let heap, v = expr env heap e2 in
      assign env heap b root v
  | Assign (Some op, e1, e2) ->
      expr env heap (Assign (None, e1, Binop (op, e1, e2)))

  | Id ("true",_) -> heap, Vbool true
  | Id ("false",_) -> heap, Vbool false
  | Id ("null",_) -> heap, Vnull

  | Infix _ | Postfix _ -> (* TODO *) heap, Vany
  | Id _ | Array_get _ | Class_get (_, _)|Obj_get (_, _)|This as lv ->
      let heap, _, x = lvalue env heap lv in
      let heap, x = Ptr.get heap x in
      heap, x

  | Lambda _ -> heap, Vany (* TODO *)

and array_value env id heap x =
  match x with
  | Aval e ->
      let heap, new_, ar = lvalue env heap id in
      let heap, a = Ptr.get heap ar in
      (match a with
      | _ when new_ ->
          let l = [] in
          let heap, v = Ptr.new_ heap in
          let l = v :: l in
          let heap = Ptr.set heap ar (Varray l) in
          let heap, e = expr env heap e in
          let heap, _ = assign env heap true v e in
          heap
      | Varray l ->
          let heap, v = Ptr.new_ heap in
          let l = v :: l in
          let heap = Ptr.set heap ar (Varray l) in
          let heap, e = expr env heap e in
          let heap, _ = assign env heap true v e in
          heap
      | _ ->
          let heap, _ = expr env heap (Assign (None, Array_get (id, None), e)) in
          heap
      )
  | Akval (e1, e2) ->
      let heap, new_, ar = lvalue env heap id in
      let heap, a = Ptr.get heap ar in
      let heap, k = expr env heap e1 in
      let heap, k = Ptr.get heap k in
      (match a, k with
      | _, Vstring k when new_ ->
          let heap, v = array_new_entry env heap ar a k SMap.empty in
          let heap, e2 = expr env heap e2 in
          let heap, _ = assign env heap true v e2 in
          heap
      | Vrecord m, Vstring k ->
          let heap, v = array_new_entry env heap ar a k m in
          let heap, e2 = expr env heap e2 in
          let heap, _ = assign env heap true v e2 in
          heap
      | _ ->
          let heap, _ = expr env heap (Assign (None, Array_get (id, Some e1), e2)) in
          heap
      )

and lvalue env heap x =
  match x with
  (* ---------------------------------------------------------------------- *)
  (* Tainting *)
  (* ---------------------------------------------------------------------- *)
  | Id ("$_POST" | "$_GET" | "$_REQUEST" as s, _) ->
      let heap, k = Ptr.new_val heap (Vtaint s) in
      let heap, v = Ptr.new_val heap (Vtaint s) in
      heap, false, Vmap (k, v)


  | Id (s,_) ->
      let heap, b, x = Var.get env heap s in
      heap, b, x
  | Array_get (e, k) ->
      let heap, b, x = array_get env heap e k in
      heap, b, x
  | ConsArray l as e ->
      let heap, a = expr env heap e in
      let heap, v = Ptr.new_ heap in
      let heap, _ = assign env heap true v a in
      heap, true, v
  | This -> lvalue env heap (Id (w "$this"))
  | Obj_get (e, Id (s,_)) ->
      let heap, v = expr env heap e in
      let heap, v' = Ptr.get heap v in
      let m = obj_get ISet.empty env heap [v'] s in
      (try
          heap, false, SMap.find s m
        with Not_found -> try
          heap, false, SMap.find ("$"^s) m
        with Not_found ->
          let heap, k = Ptr.new_val heap Vnull in
          let heap = Ptr.set heap v' (Vobject (SMap.add s k m)) in
          heap, true, k
      )
  | Class_get (c, Id (s,_)) ->
      let c = get_class env heap c in
      let heap = lazy_class env heap c in
      let heap, _, v = Var.get_global env heap c in
      let heap, v = Ptr.get heap v in
      let heap, v = Ptr.get heap v in
      (try match v with
      | Vobject m when SMap.mem s m ->
          heap, false, SMap.find s m
      | _ ->
          heap, false, Vany
        with Not_found -> heap, false, Vany)
  | Class_get (_, e) ->
      let heap, _ = expr env heap e in
      (*        failwith "TODO Obj_get" (* of expr * string *) *)
      heap, false, Vany
  | List _ -> raise Really
  | e -> heap, false, Vany
      (*        App.expr (Pp.empty print_string) e;
                failwith "TODO rest of lvalue" *)

and array_get env heap e k =
  let heap, new_, ar = lvalue env heap e in
  let heap, ar = Ptr.get heap ar in
  let heap, a = Ptr.get heap ar in
  let heap, k = Utils.opt (expr env) heap k in
  let heap, k = Utils.opt Ptr.get heap k in
  match a, k with
  | Vrecord m, Some (Vstring k) when SMap.mem k m ->
      heap, false, SMap.find k m
  | Vrecord m, Some (Vstring k) ->
      let heap, v = array_new_entry env heap ar a k m in
      heap, false, v
  | Varray l, Some (Vint k) when k >= 0 && k < List.length l ->
      heap, false, List.nth (List.rev l) k
  | Vmap (k, v), Some k' ->
      let heap, _ = Unify.value heap k k' in
      heap, false, v
  | Vmap (_, v), None ->
      heap, false, v
  | _, kval ->
      let kval = match kval with None -> Vabstr Tint | Some v -> v in
      let heap, kr = Ptr.new_ heap in
      let heap, k = Ptr.get heap kr in
      let heap = Ptr.set heap k kval in
      let heap, v = Ptr.new_ heap in
      let a' = Vmap (kr, v) in
      let heap, a = Unify.value heap a a' in
      let heap = Ptr.set heap ar a in
      heap, false, v

and binaryOp env heap bop v1 v2 =
  match bop with
  | Ast_php.Arith aop ->
      (match v1, v2 with
      | (Vint _ | Vabstr Tint), (Vint _ | Vabstr Tint) ->
          Vabstr Tint
      | _ ->
          Vsum [Vnull; Vabstr Tint]
      )
  | Ast_php.Logical lop -> Vabstr Tbool

  (* ---------------------------------------------------------------------- *)
  (* Tainting *)
  (* ---------------------------------------------------------------------- *)
  | Ast_php.BinaryConcat _ ->
      Taint.binary_concat env heap v1 v2 !path

and unaryOp uop v =
  match uop, v with
  | Ast_php.UnPlus, Vint n       -> Vint n
  | Ast_php.UnPlus, Vabstr Tint  -> Vabstr Tint
  | Ast_php.UnPlus, _            -> Vsum [Vnull; Vabstr Tint]
  | Ast_php.UnMinus, Vint n      -> Vint (-n)
  | Ast_php.UnMinus, Vabstr Tint -> Vabstr Tint
  | Ast_php.UnMinus, _           -> Vsum [Vnull; Vabstr Tint]
  | Ast_php.UnBang, Vbool b      -> Vbool (not b)
  | Ast_php.UnBang, Vabstr Tbool -> Vabstr Tbool
  | Ast_php.UnBang, _            -> Vsum [Vnull; Vabstr Tbool]
  | Ast_php.UnTilde, Vint n      -> Vint (lnot n)
  | Ast_php.UnTilde, Vabstr Tint -> Vabstr Tint
  | Ast_php.UnTilde, _           -> Vsum [Vnull; Vabstr Tint]

and sum_call env heap v el =
  (match v with
  | [] -> heap, Vany
  | Vstring s :: _ ->
      let heap, r = expr env heap (Call (Id (w s), el)) in
      heap, r
  | Vmethod (_, fm) :: _ ->
      let fl = IMap.fold (fun _ y acc -> y :: acc) fm [] in
      call_methods env heap fl el
  | Vtaint _ as v :: _ -> heap, v
  | _ :: rl -> sum_call env heap rl el
  )

and call env heap v el =
  match v with
  | Vsum l -> sum_call env heap l el
  | x -> sum_call env heap [x] el

and call_fun f env heap el =
  let is_clean =
    (let _, vl = Utils.lfold (expr env) heap el in
     List.fold_left (fun acc x -> 
       Taint.GetTaint.value heap x = None && acc) 
       true vl)
  in
  let n = try SMap.find (unw f.f_name) env.stack with Not_found -> 0 in
  let env = { env with stack = SMap.add (unw f.f_name) (n+1) env.stack } in
  save_path env (unw f.f_name);
  if n >= 2 || List.length !path >= 6 && is_clean
  (* || Sys.time() -. !time >= 1.0|| SMap.mem f.f_name !(env.safe) *)
  then
    let heap, v = Ptr.new_ heap in
    let heap, _ = assign env heap true v Vany in
    heap, v
  else
    let env = { env with vars = ref !(env.vars); cfun = unw f.f_name } in
    let heap = parameters env heap f.f_params el in
    let vars = fun_nspace f !(env.vars) in
    let env = { env with vars = ref vars } in
    path := unw f.f_name :: !path;
    let heap = stmtl env heap f.f_body in
    let heap, _, r = Var.get env heap "*return*" in
    let heap, r = Ptr.get heap r in
    path := List.tl !path;
    if Taint.GetTaint.value heap r = None
    then begin
      env.safe := SMap.add (unw f.f_name) r !(env.safe);
    end;
    (*      Printf.printf "Now Leaving %s\n" f.f_name; flush stdout; *)
    heap, r

and get_function env heap f e =
  if f.[0] = '$' then
    let heap, v = expr env heap e in
    get_dynamic_function env heap v
  else try
      heap, env.db.funs f
    with Not_found ->
      raise (UnknownFunction f)

and parameters env heap l1 l2 =
  match l1, l2 with
  | [], _ -> heap
  | p :: rl, [] ->
      (match p.p_default with
      | None -> parameters env heap rl []
      | Some e ->
          let e = if p.p_ref then make_ref e else e in
          let heap, v = expr env heap e in
          Var.unset env (unw p.p_name);
          let heap, _, lv = lvalue env heap (Id p.p_name) in
          let heap, _ = assign env heap true lv v in
          parameters env heap rl []
      )
  | p :: rl, e :: rl2 ->
      let e = if p.p_ref then make_ref e else e in
      let heap, v = expr env heap e in
      Var.unset env (unw p.p_name);
      let heap, _, lv = lvalue env heap (Id p.p_name) in
      let heap, _ = assign env heap true lv v in
      parameters env heap rl rl2

and xhp env heap x =
  match x with
  | XhpText _ -> heap
  | XhpExpr e ->
      let heap, _ = expr env heap e in
      heap
  | XhpXml x -> xml env heap x

and xhp_attr env heap x =
  match x with
  | AttrString el ->
      let heap, vl = Utils.lfold (encaps env) heap el in
      let heap, vl = Utils.lfold Ptr.get heap vl in
      let v = Taint.fold_slist vl in
      Taint.check_danger env heap "xhp attribute" (Ast_php.fakeInfo "") !path v;
      heap
  | AttrExpr e -> fst (expr env heap e)

and xml env heap x =
  let heap = List.fold_left (
    fun heap (_, x) -> xhp_attr env heap x
  ) heap x.xml_attrs in
  let heap = List.fold_left (xhp env) heap x.xml_body in
  heap

and encaps env heap x =
  match x with
  | EncapsString s -> heap, Vstring s
  | EncapsVar e
  | EncapsCurly e
  | EncapsDollarCurly e
  | EncapsExpr e -> expr env heap e

and cast env heap ty v =
  match ty, v with
  | Ast_php.BoolTy, Vbool _ -> v
  | Ast_php.BoolTy, Vabstr Tbool -> v
  | Ast_php.IntTy, Vint _ -> v
  | Ast_php.IntTy, Vabstr Tint -> v
  | Ast_php.DoubleTy, Vfloat _ -> v
  | Ast_php.DoubleTy, Vabstr Tfloat -> v
  | Ast_php.StringTy, Vstring _ -> v
  | Ast_php.StringTy, Vabstr Tstring -> v
  | Ast_php.ArrayTy, (Varray _ | Vrecord _) -> v
  | _ -> v

and global env heap v =
  match v with
  | Id (x,_) ->
      let heap, _, gv = Var.get_global env heap x in
      Var.set env x gv;
      heap
  | _ ->
      raise (Todo  "rest of global")

and static_var env heap (var, eopt) =
  let gvar = env.cfun ^ "**" ^ (unw var) in
  let heap, new_, gval = Var.get_global env heap gvar in
  let heap, _, v = Var.get env heap (unw var) in
  let heap, _ = assign env heap new_ v gval in
  match eopt with
  | None -> heap
  | Some e when new_ ->
      let heap, e = expr env heap e in
      let heap, _ = assign env heap new_ gval e in
      heap
  | Some _ -> heap

(* ---------------------------------------------------------------------- *)
(* CLASSES *)
(* ---------------------------------------------------------------------- *)

and class_def env heap c =
  (*    Printf.printf "Defining %s\n" c.c_name; flush stdout; *)
  let heap, self = Ptr.new_ heap in
  let heap, pname, parent =
    match c.c_extends with
    | [p] ->
        let heap = lazy_class env heap p in
        let heap, _, ptr = Var.get_global env heap p in
        heap, p, ptr
    | _ ->
        let heap, ptr = Ptr.new_ heap in
        heap, "", ptr
  in
  let heap, ddparent = Ptr.get heap parent in
  let heap, ddparent = Ptr.get heap ddparent in
  let m = match ddparent with Vobject m -> m | _ -> SMap.empty in
  let heap, m = List.fold_left (cconstants env) (heap, m) c.c_constants in
  let heap, m = List.fold_left (class_vars env true) (heap, m) c.c_variables in
  let heap, m = List.fold_left (method_def env true c.c_name parent self None)
    (heap, m) c.c_methods in
  let m = SMap.add "*BUILD*" (build_new env heap pname parent self c m) m in
  let v = Vobject m in
  let heap, _ = assign env heap true self v in
  heap, self

and build_new env heap pname parent self c m =
  let mid = Utils.fresh() in
  let f = build_new_ env heap pname parent self c m in
  Vmethod (Vnull, IMap.add mid f IMap.empty)

and build_new_ env heap pname parent self c m = fun env heap _ ->
  let heap, dparent = Ptr.get heap parent in
  let heap, dparent = Ptr.get heap dparent in
  let heap, ptr =
    match dparent with
    | Vobject x ->
        (match SMap.find "*BUILD*" x with
        | Vmethod (_, f) ->
            let fl = IMap.fold (fun _ y acc -> y :: acc) f [] in
            let heap, x = call_methods env heap fl [] in
            heap, x
        | _ -> assert false)
    | _ -> Ptr.new_ heap
  in
  let heap, up = Ptr.get heap ptr in
  let heap, up = Ptr.get heap up in
  let m = match up with Vobject m' -> SMap.fold SMap.add m' m | _ -> m in
  let heap, m' =
    List.fold_left (class_vars env false) (heap, m) c.c_variables in
  let heap, m' =
    List.fold_left (method_def env false c.c_name parent self (Some ptr))
      (heap, m') c.c_methods in
  let heap, _ = assign env heap true ptr (Vobject m') in
  heap, ptr

and cconstants env (heap, m) (s, e) =
  let heap, v = expr env heap e in
  heap, SMap.add s v m

and class_vars env static (heap, m) cv =
  if static then
    if not cv.cv_static
    then heap, m
    else List.fold_left (class_var env static) (heap, m) cv.cv_vars
  else
    if cv.cv_static
    then heap, m
  else  List.fold_left (class_var env static) (heap, m) cv.cv_vars

and class_var env static (heap, m) (s, e) =
  let s = if static then s else String.sub s 1 (String.length s - 1) in
  match e with
  | None ->
      let heap, v = Ptr.new_ heap in
      heap, SMap.add s v m
  | Some e ->
      let heap, v1 = Ptr.new_ heap in
      let heap, v2 = expr env heap e in
      let heap, _ = assign env heap true v1 v2 in
      heap, SMap.add s v1 m

and method_def env static cname parent self this (heap, acc) m =
  method_def_ env cname parent self this (heap, acc) m

and method_def_ env cname parent self this (heap, acc) m =
  let fdef = {
    f_ref = false;
    f_name = w (unw cname ^ "::" ^ unw m.m_name);
    f_params = m.m_params;
    f_return_type = m.m_return_type;
    f_body = m.m_body;
  } in
  let cls = make_method m.m_name parent self this fdef in
  let mid = Utils.fresh() in
  let v = match this with None -> Vnull | Some v -> v in
  let v = Vmethod (v, IMap.add mid cls IMap.empty) in
  heap, SMap.add (unw m.m_name) v acc


and make_method mname parent self this fdef =
  fun env heap el ->
    let old_self =
      try Some (SMap.find "self" !(env.globals)) with Not_found -> None in
    let old_parent =
      try Some (SMap.find "parent" !(env.globals)) with Not_found -> None in
    let old_this =
      try Some (SMap.find "$this" !(env.globals)) with Not_found -> None in
    Var.set_global env "self" self;
    Var.set_global env "parent" parent;
    (match this with
    | None -> ()
    | Some v -> Var.set_global env "$this" v
    );
    let heap, res = call_fun fdef env heap el in
    let heap, res' = Ptr.get heap res in
    let heap, res' = Ptr.get heap res' in
    if unw mname = "render"
    then Taint.check_danger env heap "return value of render" (snd mname) 
      !path res';
    (match old_self with Some x -> Var.set_global env "self" x | None -> ());
    (match old_parent with Some x -> Var.set_global env "parent" x | None ->());
    (match old_this with Some x -> Var.set_global env "$this" x | None -> ());
    heap, res

and call_methods env heap fl el =
  match fl with
  | [] -> assert false
  | [f] -> f env heap el
  | f1 :: rl ->
      let heap, v = f1 env heap el in
      List.fold_left (call_method env el) (heap, v) rl

and call_method env el (heap, v) f =
  let heap, v' = f env heap el in
  let heap, v = Unify.value heap v v' in
  heap, v


and lazy_class env heap c =
  try
    if not (SMap.mem c !(env.globals)) && 
      (try 
          let _ = env.db.classes c in
          true
        with Not_found -> false
      )
    then force_class env heap c
    else heap
  with Not_found -> heap

and force_class env heap c =
  let c = env.db.classes c in
  let heap, null = Ptr.new_ heap in
  Var.set_global env (unw c.c_name) null;
  let heap, cd = class_def env heap c in
  Var.set_global env (unw c.c_name) cd;
  heap

and get_class env heap c =
  match c with
  | Id ("",_) -> ""
  | Id (s,_) when s.[0] <> '$' -> s
  | Id _ ->
      let env, v = expr env heap c in
      let heap, v = Ptr.get heap v in
      let heap, v = Ptr.get heap v in
      get_string [v]
  | _ -> ""

and get_string = function
  | [] -> ""
  | Vstring s :: _ -> s
  | Vsum l' :: rl ->
      (match get_string l' with
      | "" -> get_string rl
      | x -> x
      )
  | _ :: rl -> get_string rl
