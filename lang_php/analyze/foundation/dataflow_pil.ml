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

open Pil

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
(* not used anymore *)
module NodeiMap = Map.Make(NiOrd)


(* The final dataflow result; a map from each program point to a map containing
 * information from each variables.
 *
 * opti: this used to be a 'NodeiMap.t' instead of an 'array' but 'nodei'
 * are always int and array gives a 6x speedup according to iain
 * so let's use array.
 *)
type 'a mapping = ('a inout) array

  and 'a inout = {
    in_env: 'a env;
    out_env: 'a env;
  }
   and 'a env = 'a VarMap.t

let empty_env () = VarMap.empty
let empty_inout () = {in_env = empty_env (); out_env = empty_env ()}

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)


let eq_env eq e1 e2 =
  VarMap.equal eq e1 e2
let eq_inout eq io1 io2 =
  let eqe = eq_env eq in
  (eqe io1.in_env io2.in_env) && (eqe io1.out_env io2.out_env)

exception Break

let eq_mapping eq m1 m2 =
  try (
    for x = 0 to Array.length m1 do
      if not (eq_inout eq m1.(x) m2.(x)) then raise Break
    done;
    true
  )
  with Break -> false


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

let (env_to_str: ('a -> string) -> 'a env -> string) =
fun val2str env ->
  VarMap.fold
    (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ")
    env ""

let (inout_to_str: ('a -> string) -> 'a inout -> string) =
fun val2str inout ->
  spf "IN= %15s  OUT = %15s"
    (env_to_str val2str inout.in_env)
    (env_to_str val2str inout.out_env)

let array_fold_left_idx f = let idx = ref 0 in
  Array.fold_left (fun v e -> let r = f v !idx e in incr idx; r)

let mapping_to_str (fl : F.flow) val2str mapping =
  array_fold_left_idx (fun s ni v -> s ^
    (spf "%2d <- %7s: %15s %s\n"
      ni
      ((fl#predecessors ni)#fold (fun s (ni, _) ->
      csv_append s (string_of_int ni)) "")
     (Controlflow_pil.short_string_of_node (fl#nodes#find ni))
     (inout_to_str val2str v)
  )) "" mapping

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
type 'a transfn = 'a mapping -> Controlflow_pil.nodei -> 'a inout

let rec fixpoint_worker eq mp trans flow succs work =
  if NodeiSet.is_empty work then mp else
  let ni = NodeiSet.choose work in
  let work' = NodeiSet.remove ni work in
  let old = mp.(ni) in
  let nu = trans mp ni in
  let work'' = if eq_inout eq old nu
               then work'
               else (mp.(ni) <- nu; NodeiSet.union work' (succs flow ni)) in
  fixpoint_worker eq mp trans flow succs work''


let forward_succs (f : F.flow) n = (f#successors n)#fold
  (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty
let backward_succs (f : F.flow) n = (f#predecessors n)#fold
  (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty

let (fixpoint:
    eq:('a -> 'a -> bool) ->
    init:'a mapping ->
    trans:'a transfn ->
    flow:F.flow ->
    forward: bool ->
   'a mapping) =
 fun ~eq ~init ~trans ~flow ~forward ->
  fixpoint_worker eq init trans flow
   (if forward then forward_succs else backward_succs)
   (flow#nodes#fold (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty)

(*****************************************************************************)
(* Node Visitors *)
(*****************************************************************************)

(* TODO: should move this code in visitor_pil.ml and should use
 * ocamltarzan and an iterative visitor instead.
 *)

(* Node id -> var name -> is on the lhs? -> acc -> acc' *)
type 'a fold_fn = nodei -> string -> bool -> 'a -> 'a

type 'a fold_env = {fold_fn: string -> bool -> 'a -> 'a; fold_vars: VarSet.t}

let (lv_fold : 'a fold_env -> bool -> Pil.lvaluebis -> 'a
    -> 'a) =
fun fold_env lhs lvl acc -> match lvl with
  | Pil.VVar (Pil.Var va)
  | Pil.ArrayAccess (Pil.Var va, _)
  | Pil.ObjAccess (Pil.Var va, _)
  | Pil.DynamicObjAccess (Pil.Var va, _) ->
    fold_env.fold_fn (Ast_php.dname va) lhs acc
  | Pil.IndirectAccess (Var va, _) ->
    VarSet.fold (fun var acc' -> fold_env.fold_fn var lhs acc')
      fold_env.fold_vars (fold_env.fold_fn (Ast_php.dname va) false acc)

  | VVar (This _)
  | ArrayAccess (This _, _)
  | ObjAccess (This _, _)
  | DynamicObjAccess (This _, _)
  | IndirectAccess (This _, _)
  | VQualifier (_, _) 
  | TodoLvalue _
    -> acc (* TODO ? *)

let rec (expr_fold: 'a fold_env -> Pil.exprbis -> 'a -> 'a) =
fun fold_env exp acc -> let rcr = expr_fold fold_env in
  match exp with
  | Pil.Lv (lvl, _) -> lv_fold fold_env false lvl acc
  | Pil.Binary ((e1,_), _, (e2,_)) -> rcr e2 (rcr e1 acc)
  | Pil.Cast (_, (e,_))
  | Pil.InstanceOf ((e,_), _)
  | Pil.Unary (_, (e,_)) -> rcr e acc
  | Pil.CondExpr ((e1,_), (e2,_), (e3,_)) -> rcr e3 (rcr e1 (rcr e2 acc))
  | Pil.ConsArray es -> List.fold_left (fun acc' (e,_) -> rcr e acc') acc es
  | Pil.ConsHash eps -> List.fold_left (fun acc' ((e1,_), (e2,_)) ->
       rcr e2 (rcr e1 acc')) acc eps

  | (ClassConstant _|C _) -> acc (* TODO ? *)
  | Pil.TodoExpr _ -> acc


let (instr_fold: 'a fold_env -> Pil.instr -> 'a -> 'a) =
fun fold_env inst acc ->
  let lvf = lv_fold fold_env true in
  let exf = expr_fold fold_env in
  match inst with
  | Pil.Assign ((lv, _), _, (e,_)) -> lvf lv (exf e acc)
  | Pil.AssignRef ((lv1, _), (lv2, _)) -> lvf lv1 (lvf lv2 acc)
  | Pil.Call ((lv, _), _, args) -> lvf lv
    (List.fold_left (fun acc' -> function
                     | Pil.Arg (e, _) -> exf e acc'
                     | Pil.ArgRef (lv, _) -> lvf lv acc')
      acc args)
  | Pil.Eval (e, _) -> exf e acc
  | TodoInstr _ -> acc

let (node_fold: 'a fold_env -> F.node_kind -> 'a -> 'a) =
fun fold_env node acc -> match node with
  | F.Instr i -> instr_fold fold_env i acc
  | F.WhileHeader (e,_)
  | F.Return (Some (e, _))
  | F.IfHeader (e, _) -> expr_fold fold_env e acc
  | F.Echo es -> List.fold_left (fun acc' (e, _) -> expr_fold fold_env e acc')
    acc es

  | (F.Join|F.Throw|F.TryHeader|F.Jump|F.FalseNode|F.TrueNode|F.Exit|F.Enter)
  | F.Return (None)
  | F.TodoNode _
    ->
      acc (* TODO *)

let (flow_fold: 'a fold_fn -> VarSet.t -> 'a -> F.flow -> 'a) =
fun fold_fn vars acc flow -> flow#nodes#fold
  (fun acc' (ni, nd) -> node_fold {fold_fn = fold_fn ni; fold_vars = vars}
   nd.F.n acc') acc

let flow_fold_lv fold_fn vars acc flow = flow_fold
  (fun ndi dnm lhs acc' -> if lhs then fold_fn ndi dnm acc' else acc') vars acc
    flow
let flow_fold_rv fold_fn vars acc flow = flow_fold
  (fun ndi dnm lhs acc' -> if not lhs then fold_fn ndi dnm acc' else acc')
    vars acc flow



let new_node_array (f: F.flow) v =
  let arr = Array.make f#nb_nodes v in
  (* sanity checking *)
  let len = Array.length arr in
  f#nodes#tolist +> List.iter (fun (ni, nod) ->
    if ni >= len
    then failwith "the CFG nodei is bigger than the number of nodes"
  );

  arr


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
  flow_fold (fun _ va _ vs -> VarSet.add va vs) VarSet.empty VarSet.empty

let (reaching_defs: VarSet.t -> F.flow -> NodeiSet.t env) = fun vars ->
  flow_fold_lv (fun ni va acc -> add_def acc va ni) vars VarMap.empty

let up_map k f mp = mp.(k) <- f mp.(k); mp

let up_set_map k v mp = up_map k (VarSet.add v) mp

(* gen/kill *)
let (reaching_gens: VarSet.t -> F.flow -> VarSet.t array) = fun vars fl ->
  flow_fold_lv (fun ni v gs -> up_set_map ni v gs) vars
    (new_node_array fl VarSet.empty) fl

let (reaching_kills:
   NodeiSet.t env -> VarSet.t -> F.flow -> (NodeiSet.t env) array) =
 fun ds vars fl -> flow_fold_lv (fun ni va ks ->
    let d = NodeiSet.remove ni (VarMap.find va ds) in
    up_map ni (fun v -> VarMap.add va d v) ks) vars
      (new_node_array fl (empty_env())) fl

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
   gen:VarSet.t array ->
   kill:(NodeiSet.t env) array ->
   flow:F.flow ->
   NodeiSet.t transfn) =
 fun ~gen ~kill ~flow ->
  fun mp ni ->

  let new_in = (flow#predecessors ni)#fold (fun s (nip, _) ->
      add_env s mp.(nip).out_env) VarMap.empty in
  let in_k = minus_env new_in kill.(ni) in
  let new_out = VarSet.fold (fun v e -> VarMap.add v
      (try NodeiSet.add ni (VarMap.find v e)
       with Not_found -> NodeiSet.singleton ni) e)
    gen.(ni) in_k in
  {in_env = new_in; out_env = new_out}

let (reaching_fixpoint: F.flow -> reaching_mapping) = fun flow ->
  let vars = vars flow in
  let defs = reaching_defs vars flow in
  let gen = reaching_gens vars flow in
  let kill = reaching_kills defs vars flow in

  fixpoint
    ~eq:NodeiSet.equal
    ~init:(new_node_array flow (empty_inout ()))
    ~trans:(reaching_transfer ~gen ~kill ~flow)
    ~forward:true
    ~flow

(*****************************************************************************)
(* Example of analysis: liveness analysis *)
(*****************************************************************************)

type liveness_mapping = unit mapping

let (liveness_gens: VarSet.t -> F.flow -> VarSet.t array) = fun vars fl ->
  flow_fold_rv (fun ni va gs -> up_set_map ni va gs) vars
    (new_node_array fl VarSet.empty) fl

let (liveness_kills: VarSet.t -> F.flow -> (unit env) array) = fun vars fl ->
  flow_fold_lv (fun ni va ks ->
      up_map ni (fun v -> VarMap.add va () v) ks) vars
    (new_node_array fl (empty_env ())) fl

let (liveness_transfer:
    gen: VarSet.t array ->
    kill: (unit env) array ->
    flow: F.flow ->
    unit transfn) =
 fun ~gen ~kill ~flow ->
  fun mp ni ->
  let new_out =
    (flow#successors ni)#fold (fun s (nip, _) ->
        VarMap.fold (fun v _ m -> VarMap.add v () m) s mp.(nip).in_env)
    VarMap.empty in
  let out_k = VarMap.fold (fun v _ m -> VarMap.remove v m) kill.(ni) new_out in
  let new_in = VarSet.fold (fun v e -> VarMap.add v () e) gen.(ni) out_k in
  {in_env = new_in; out_env = new_out}

let (liveness_fixpoint: F.flow -> liveness_mapping) = fun flow ->
  let vars = vars flow in
  let gen = liveness_gens vars flow in
  let kill = liveness_kills vars flow in
  fixpoint
    ~eq:(fun _ _ -> true)
    ~init:(new_node_array flow (empty_inout ()))
    ~trans:(liveness_transfer ~gen ~kill ~flow)
    ~forward:false
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
