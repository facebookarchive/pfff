(*s: dataflow_pil.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau, Iain Proctor
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

module DnOrd = struct
  type t = Pil.dname
  let compare dn1 dn2 = compare (Ast_php.dname dn1) (Ast_php.dname dn2)
end
module DM = Map.Make(DnOrd)
module DS = Set.Make(DnOrd)


type ni = F.nodei
module NiOrd = struct
  type t = ni
  let compare = compare
end

module NS = Set.Make(NiOrd)
module NM = Map.Make(NiOrd)


type 'a env = 'a DM.t

type 'a inout = {
  in_env: 'a env;
  out_env: 'a env;
}

type 'a mapping = ('a inout) NM.t

let empty_env = DM.empty
let empty_inout = {in_env = empty_env; out_env = empty_env}

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)
let eq_env eq e1 e2 = DM.equal eq e1 e2
let eq_inout eq io1 io2 = let eqe = eq_env eq in
  eqe io1.in_env io2.in_env && eqe io2.out_env io2.out_env
let eq_mapping eq m1 m2 = NM.equal (eq_inout eq) m1 m2

let csv_append s v =
  if String.length s == 0 then v else s ^ "," ^ v


(*****************************************************************************)
(* Env manipulation *)
(*****************************************************************************)

let (minus_env : NS.t env ->  NS.t env -> NS.t env) =
fun e1 e2 -> DM.fold (fun v s e' ->
  try
    let df = NS.diff (DM.find v e') s in
    if NS.is_empty df then DM.remove v e' else DM.add v df e'
  with Not_found -> e')
 e2 e1

let (add_env : NS.t env -> NS.t env -> NS.t env) =
fun e1 e2 -> DM.fold (fun v s e' ->
    let s2 = try NS.union s (DM.find v e') with Not_found -> s in
      DM.add v s2 e')
  e2 e1

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*
 * todo? having only a transfer function is enough ? do we need to pass
 * extra information to it ? maybe only the mapping is not enough. For
 * instance if in the code there is $x = &$g, a reference, then
 * we may want later to have access to this information. Maybe we
 * should pass an extra env argument ? Or maybe can encode this
 * sharing of reference in the 'a, so that when one update the
 * value associated to a var, its reference variable get also
 * the update.
 *)


let rec (fixpoint:
      F.flow -> ('a -> 'a -> bool) -> 'a mapping ->
      ('a mapping -> Ograph_extended.nodei -> 'a mapping) -> 'a mapping) =
 fun flow eq init trans ->
   let nu = flow#nodes#fold (fun m (ni, _) -> trans m ni) init in
   if eq_mapping eq nu init then nu else fixpoint flow eq nu trans

type 'a flow_cb = 'a -> ni -> Pil.dname -> 'a

let (lv_fold : 'a flow_cb -> 'a -> ni -> Pil.lvaluebis -> 'a) =
fun lf v ni -> function
  | Pil.VVar (Pil.Var va)
  | Pil.ArrayAccess (Pil.Var va, _)
  | Pil.ObjAccess (Pil.Var va, _)
  | Pil.DynamicObjAccess (Pil.Var va, _) -> lf v ni va
  | Pil.IndirectAccess _ -> raise Todo
  | _ -> v

let (instr_fold: 'a flow_cb -> 'a -> ni -> Pil.instr -> 'a) =
fun lf v ni -> function
  | Pil.Assign ((lv, _), _, _) -> lv_fold lf v ni lv
  | Pil.AssignRef ((lv1, _), (lv2, _)) ->
    lv_fold lf (lv_fold lf v ni lv1) ni lv2
  | Pil.Call ((lv, _), _, args) -> lv_fold lf v ni lv
  | _ -> v

let (node_fold: 'a flow_cb -> 'a -> ni -> F.node_kind -> 'a) =
fun lf v ni -> function
  | F.Instr i -> instr_fold lf v ni i
  | _ -> v

let (flow_fold: 'a flow_cb -> 'a -> F.flow -> 'a) =
fun lf v flow -> flow#nodes#fold
  (fun v' (ni, nd) -> node_fold lf v' ni nd.F.n) v

(*****************************************************************************)
(* Reaching definitions *)
(*****************************************************************************)

let add_def d nm ni =
  let v = try NS.add ni (DM.find nm d) with Not_found -> NS.singleton ni
  in
    DM.add nm v d

let (vars: F.flow -> DS.t) = flow_fold (fun vs ni v -> DS.add v vs) DS.empty
let (reaching_defs: F.flow -> NS.t env) = flow_fold
  (fun ds ni v -> add_def ds v ni) DM.empty

let up_map k f mp =
  let v = if NM.mem k mp then f (Some (NM.find k mp)) else f None in
    NM.add k v mp

let (reaching_gens: F.flow -> DS.t NM.t) = flow_fold (fun gs ni v ->
    up_map ni (function None -> DS.singleton v | Some s -> DS.add v s) gs)
  NM.empty

let (reaching_kills: NS.t env -> F.flow -> (NS.t DM.t) NM.t) =
fun ds -> flow_fold (fun ks ni va ->
    let d = NS.remove ni (DM.find va ds) in
      up_map ni (function None -> DM.add va d DM.empty
                        | Some v -> DM.add va d v)
      ks)
  NM.empty

(**
 * This algorithm is taken from Modern Compiler Implementation in ML, Appel,
 * 1998, pp. 382.
 * The transfer is setting in'[n] = U_{p in pred[n]} out[p] and
 * out'[n] = gen[n]U(in[n] - kill[n]) where gen[n] is {n} if there in a
 * definition at n and {} otherwise, and kill[n] is the set of all definitions
 * of the variable being defined at n except the one at n.
 *)
let (reaching_transfer: F.flow -> DS.t NM.t -> (NS.t DM.t) NM.t ->
    NS.t mapping -> ni -> NS.t mapping) =
fun fl gs ks mp ni ->
  let io = try NM.find ni mp with Not_found -> empty_inout in
  let in_k = minus_env io.in_env
    (try NM.find ni ks with Not_found -> empty_env) in
  NM.add ni
    {in_env = (fl#predecessors ni)#fold
      (fun s (nip, _) -> add_env s
         (try NM.find nip mp with Not_found -> empty_inout).out_env)
      DM.empty;
        out_env = try DS.fold (fun v e ->
                                 DM.add v (try NS.add ni (DM.find v e)
                                          with Not_found -> NS.singleton ni) e)
                        (NM.find ni gs) in_k
                  with Not_found -> in_k}
    mp

let (reaching_fixpoint: F.flow -> NS.t mapping) =
fun fl ->
  let gs = reaching_gens fl in
  let ds = reaching_defs fl in
  let ks = reaching_kills ds fl in
  fixpoint fl NS.equal NM.empty (reaching_transfer fl gs ks)

(*****************************************************************************)
(* Debugging support *)
(*****************************************************************************)

let ns_to_str ns = "{" ^
  NS.fold (fun n s -> csv_append s (string_of_int n)) ns "" ^ "}"
let env_to_str val2str env =
  DM.fold (fun dn v s -> s ^ Ast_php.dname dn ^ ":" ^ val2str v ^ " ") env ""

let inout_to_str val2str inout = env_to_str val2str inout.in_env ^ " | " ^
  env_to_str val2str inout.out_env

let mapping_to_str (fl : F.flow) val2str mapping = NM.fold (fun ni v s -> s ^
  string_of_int ni ^ "<-" ^
  (fl#predecessors ni)#fold (fun s (ni, _) ->
    csv_append s (string_of_int ni)) "" ^
    ":" ^
    Controlflow_pil.short_string_of_node (fl#nodes#find ni) ^ ":" ^
    inout_to_str val2str v ^ "\n") mapping ""

let (display_dflow: F.flow -> 'a mapping -> ('a -> string) -> unit) =
 fun flow mapping string_of_val ->
   pr (mapping_to_str flow string_of_val mapping)

let display_reaching_dflow flow mp = display_dflow flow mp ns_to_str

(*e: dataflow_php. *)
