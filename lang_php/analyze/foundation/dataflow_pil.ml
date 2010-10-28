(*s: dataflow_pil.ml *)
(*s: Facebook copyright *)
(* Iain Proctor
 *
 * Copyright (C) 2009-2010 Facebook
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
(*e: Facebook copyright *)

open Common

module F = Controlflow_pil

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * The goal of a dataflow analysis is to store information about each
 * variable at each program point, that is each node in a CFG
 * (e.g. whether a variable is "live" at a program point).
 * As you may want different kind of information, the types below
 * are polymorphic. But each take as a key a variable name (dname, for
 * dollar name, the type of variables in Ast_php).
 *
 * less: could use a functor, so would not have all those 'a.
 * todo? do we need other kind of information than variable environment ?
 * Dataflow analysis talks only about variables ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The comparison function uses only the name string so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)
module StrOrd = struct
  type t = string
  let compare = compare
end

module VarMap = Map.Make(StrOrd)
module VarSet = Set.Make(StrOrd)

type nodei = F.nodei

module NiOrd = struct
  type t = nodei
  let compare = compare
end

module NodeiSet = Set.Make(NiOrd)
module NodeiMap = Map.Make(NiOrd)



(* The final dataflow result; a map from each program point to a map containing
 * information from each variables.
 *)
type 'a mapping = ('a inout) NodeiMap.t

  and 'a inout = {
    in_env: 'a env;
    out_env: 'a env;
  }
   and 'a env = 'a VarMap.t

let empty_env _ = VarMap.empty
let empty_inout = {in_env = empty_env (); out_env = empty_env ()}

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

let eq_env eq e1 e2 =
  VarMap.equal eq e1 e2
let eq_inout eq io1 io2 =
  let eqe = eq_env eq in
  (eqe io1.in_env io2.in_env) && (eqe io1.out_env io2.out_env)
let eq_mapping eq m1 m2 =
  NodeiMap.equal (eq_inout eq) m1 m2


(*****************************************************************************)
(* Env manipulation *)
(*****************************************************************************)

let (minus_env : NodeiSet.t env ->  NodeiSet.t env -> NodeiSet.t env) =
fun e1 e2 -> VarMap.fold (fun v s e' ->
  try
    let df = NodeiSet.diff (VarMap.find v e') s in
    if NodeiSet.is_empty df then VarMap.remove v e' else VarMap.add v df e'
  with Not_found -> e')
 e2 e1

let (add_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env) =
fun e1 e2 -> VarMap.fold (fun v s e' ->
    let s2 = try NodeiSet.union s (VarMap.find v e') with Not_found -> s in
      VarMap.add v s2 e')
  e2 e1

(*****************************************************************************)
(* Debugging support *)
(*****************************************************************************)

let csv_append s v =
  if String.length s == 0 then v else s ^ "," ^ v

let ns_to_str ns =
  "{" ^
  NodeiSet.fold (fun n s -> csv_append s (string_of_int n)) ns "" ^
  "}"

let env_to_str val2str env =
  VarMap.fold
    (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ")
    env ""

let inout_to_str val2str inout =
  spf "IN= %15s  OUT = %15s"
    (env_to_str val2str inout.in_env)
    (env_to_str val2str inout.out_env)

let mapping_to_str (fl : F.flow) val2str mapping =
  NodeiMap.fold (fun ni v s -> s ^
    (spf "%2d <- %7s: %15s %s\n"
      ni
      ((fl#predecessors ni)#fold (fun s (ni, _) ->
      csv_append s (string_of_int ni)) "")
     (Controlflow_pil.short_string_of_node (fl#nodes#find ni))
     (inout_to_str val2str v)
  ))
  mapping ""

(*****************************************************************************)
(* Main generic entry point *)
(*****************************************************************************)

(* The transition/transfer function. It is usually made from the
 * gens and kills.
 *
 * todo? having only a transfer function is enough ? do we need to pass
 * extra information to it ? maybe only the mapping is not enough. For
 * instance if in the code there is $x = &$g, a reference, then
 * we may want later to have access to this information. Maybe we
 * should pass an extra env argument ? Or maybe can encode this
 * sharing of reference in the 'a, so that when one update the
 * value associated to a var, its reference variable get also
 * the update.
 *)
type 'a transfn = 'a mapping -> Controlflow_pil.nodei -> 'a mapping

let rec (fixpoint:
    eq:('a -> 'a -> bool) ->
    init:'a mapping ->
    trans:'a transfn ->
    flow:F.flow ->
   'a mapping) =
 fun ~eq ~init ~trans ~flow ->
   let nu = flow#nodes#fold (fun m (ni, _) -> trans m ni) init in
   if eq_mapping eq nu init
   then nu
   else fixpoint ~eq ~init:nu ~trans ~flow

(*****************************************************************************)
(* ??? *)
(*****************************************************************************)

type 'a flow_cb = nodei -> string -> bool -> 'a -> 'a

let (lv_fold : 'a flow_cb -> nodei -> bool -> Pil.lvaluebis -> 'a -> 'a) =
fun lf ni islv lv v -> match lv with
  | Pil.VVar (Pil.Var va)
  | Pil.ArrayAccess (Pil.Var va, _)
  | Pil.ObjAccess (Pil.Var va, _)
  | Pil.DynamicObjAccess (Pil.Var va, _) -> lf ni (Ast_php.dname va) islv v
  | Pil.IndirectAccess _ -> raise Todo
  | _ -> v

let rec (expr_fold: 'a flow_cb -> nodei -> Pil.exprbis -> 'a -> 'a) =
fun lf ni ex v -> let r = expr_fold lf ni in
  match ex with
  | Pil.Lv (lv, _) -> lv_fold lf ni false lv v
  | Pil.Binary ((e1,_), _, (e2,_)) -> r e2 (r e1 v)
  | Pil.Cast (_, (e,_))
  | Pil.InstanceOf ((e,_), _)
  | Pil.Unary (_, (e,_)) -> r e v
  | Pil.CondExpr ((e1,_), (e2,_), (e3,_)) -> expr_fold lf ni e3 (r e1 (r e2 v))
  | Pil.ConsArray es -> List.fold_left (fun v' (e,_) -> r e v') v es
  | Pil.ConsHash eps -> List.fold_left (fun v' ((e1,_), (e2,_)) ->
       r e2 (r e1 v')) v eps
  | _ -> v

let (instr_fold: 'a flow_cb -> nodei -> Pil.instr -> 'a -> 'a) =
fun lf ni i v ->
  let lvf = lv_fold lf ni true in
  let ef = expr_fold lf ni in
  match i with
  | Pil.Assign ((lv, _), _, (e,_)) -> lvf lv (ef e v)
  | Pil.AssignRef ((lv1, _), (lv2, _)) -> lvf lv1 (lvf lv2 v)
  | Pil.Call ((lv, _), _, args) -> lvf lv
    (List.fold_left (fun v -> function Pil.Arg (e, _) -> ef e v
                                     | Pil.ArgRef (lv, _) -> lvf lv v) v args)
  | Pil.Eval (e, _) -> ef e v

let (node_fold: 'a flow_cb -> nodei -> F.node_kind -> 'a -> 'a) =
fun lf ni n v -> match n with
  | F.Instr i -> instr_fold lf ni i v
  | F.WhileHeader (e,_)
  | F.Return (Some (e, _))
  | F.IfHeader (e, _) -> expr_fold lf ni e v
  | F.Echo es -> List.fold_left (fun v' (e, _) -> expr_fold lf ni e v') v es
  | _ -> v

let (flow_fold: 'a flow_cb -> 'a -> F.flow -> 'a) =
fun lf v flow -> flow#nodes#fold
  (fun v' (ni, nd) -> node_fold lf ni nd.F.n v') v

let flow_fold_lv lf v fl = flow_fold
  (fun ni dn lv v -> if lv then lf ni dn v else v) v fl
let flow_fold_rv lf v fl = flow_fold
  (fun ni dn lv v -> if not lv then lf ni dn v else v) v fl

(*****************************************************************************)
(* Example of analysis: reaching definitions *)
(*****************************************************************************)

(* For a reaching definitions analysis the dataflow result is
 * a map from each program point (as usual), to a map from each
 * variable (as usual), to a set of nodes that define this variable
 * that are visible at this program point.
 *
 * For instance on:
 *
 * 1: $a = 1;
 * 2: if(...) {
 * 3:   $a = 2;
 * 4: } else {
 * 5:   $a = 3;
 * 6: }
 * 7: echo $a;
 *
 * then at the program point (node index) 7, then for $a the nodei set
 * is {3, 5}, but not '1'.
 *)
type reaching_mapping = NodeiSet.t mapping

let add_def d nm ni =
  let v =
    try NodeiSet.add ni (VarMap.find nm d)
    with Not_found -> NodeiSet.singleton ni
  in
  VarMap.add nm v d

let (vars: F.flow -> VarSet.t) =
  flow_fold (fun _ va _ vs -> VarSet.add va vs) VarSet.empty

let (reaching_defs: F.flow -> NodeiSet.t env) =
  flow_fold (fun ni va _ ds -> add_def ds va ni) VarMap.empty

let up_map k f mp =
  let v =
    if NodeiMap.mem k mp
    then f (Some (NodeiMap.find k mp))
    else f None
  in
  NodeiMap.add k v mp

let up_set_map k v mp =
  up_map k (function None -> VarSet.singleton v | Some s -> VarSet.add v s) mp

(* gen/kill *)
let (reaching_gens: F.flow -> VarSet.t NodeiMap.t) =
  flow_fold_lv (fun ni v gs -> up_set_map ni v gs) NodeiMap.empty

let (reaching_kills:
   NodeiSet.t env -> F.flow -> (NodeiSet.t env) NodeiMap.t) =
 fun ds -> flow_fold_lv (fun ni va ks ->
    let d = NodeiSet.remove ni (VarMap.find va ds) in
      up_map ni (function None -> VarMap.add va d VarMap.empty
                        | Some v -> VarMap.add va d v)
      ks)
  NodeiMap.empty

(*
 * This algorithm is taken from Modern Compiler Implementation in ML, Appel,
 * 1998, pp. 382.
 *
 * The transfer is setting in'[n] = U_{p in pred[n]} out[p] and
 * out'[n] = gen[n]U(in[n] - kill[n]) where gen[n] is {n} if there in a
 * definition at n and {} otherwise, and kill[n] is the set of all definitions
 * of the variable being defined at n except the one at n.
 *)
let (reaching_transfer:
   gen:VarSet.t NodeiMap.t ->
   kill:(NodeiSet.t env) NodeiMap.t ->
   flow:F.flow ->
   NodeiSet.t transfn) =
 fun ~gen ~kill ~flow ->
  fun mp ni ->

  let io =
    try NodeiMap.find ni mp with Not_found -> empty_inout
  in
  let in_k = minus_env io.in_env
    (try NodeiMap.find ni kill with Not_found -> empty_env ())
  in
  NodeiMap.add ni {
    in_env =
      (flow#predecessors ni)#fold (fun s (nip, _) ->
        add_env s
          (try NodeiMap.find nip mp with Not_found -> empty_inout).out_env
      )
      VarMap.empty;

    out_env =
      try
        VarSet.fold (fun v e ->
          VarMap.add v
            (try NodeiSet.add ni (VarMap.find v e)
             with Not_found -> NodeiSet.singleton ni) e
        )
        (NodeiMap.find ni gen)
        in_k
      with Not_found -> in_k
  }
    mp

let (reaching_fixpoint: F.flow -> reaching_mapping) = fun flow ->
  let ds = reaching_defs flow in

  let gen = reaching_gens flow in
  let kill = reaching_kills ds flow in

  fixpoint
    ~eq:NodeiSet.equal
    ~init:NodeiMap.empty
    ~trans:(reaching_transfer ~gen ~kill ~flow)
    ~flow

(*****************************************************************************)
(* Example of analysis: liveness analysis *)
(*****************************************************************************)

type liveness_mapping = unit mapping

let (liveness_gens: F.flow -> VarSet.t NodeiMap.t) =
  flow_fold_rv (fun ni va gs -> up_set_map ni va gs) NodeiMap.empty

let (liveness_kills: F.flow -> (unit env) NodeiMap.t) =
  flow_fold_lv (fun ni va ks ->
      up_map ni (function Some v -> v
                        | None -> VarMap.add va () VarMap.empty) ks)
  NodeiMap.empty

let (liveness_transfer:
    gen: VarSet.t NodeiMap.t ->
    kill: (unit env) NodeiMap.t ->
    flow: F.flow ->
    unit transfn) =
 fun ~gen ~kill ~flow ->
  fun mp ni ->
  let io = try NodeiMap.find ni mp with Not_found -> empty_inout in
  let out_k = VarMap.fold (fun v _ m -> VarMap.remove v m)
    (try NodeiMap.find ni kill with Not_found -> empty_env ()) io.out_env
  in
  let io' = {
    in_env = (try
        VarSet.fold (fun v e -> VarMap.add v () e) (NodeiMap.find ni gen)
          out_k
       with Not_found -> out_k);
    out_env =
      (flow#successors ni)#fold (fun s (nip, _) ->
          VarMap.fold (fun v _ m -> VarMap.add v () m) s
            (try NodeiMap.find nip mp with Not_found -> empty_inout).in_env
        )
      VarMap.empty
    } in
  NodeiMap.add ni io' mp

let (liveness_fixpoint: F.flow -> liveness_mapping) = fun flow ->
  let gen = liveness_gens flow in
  let kill = liveness_kills flow in
  fixpoint
    ~eq:(fun _ _ -> true)
    ~init:NodeiMap.empty
    ~trans:(liveness_transfer ~gen ~kill ~flow)
    ~flow

(*****************************************************************************)
(* Dataflow pretty printing *)
(*****************************************************************************)

let (display_dflow: F.flow -> 'a mapping -> ('a -> string) -> unit) =
 fun flow mapping string_of_val ->
   pr (mapping_to_str flow string_of_val mapping)

let display_reaching_dflow flow mp =
  display_dflow flow mp ns_to_str

let display_liveness_dflow flow mp =
  display_dflow flow mp (fun _ -> "")

(*e: dataflow_php. *)
