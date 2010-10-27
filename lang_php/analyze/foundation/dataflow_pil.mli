(*s: dataflow_pil.mli *)

(* the goal of a dataflow analysis is to store information about each
 * variable (VarMap) at each program point (NodeiMap).
 *)
module VarMap : Map.S with type key = Pil.dname
module NodeiMap : Map.S with type key = Controlflow_pil.nodei

(* the final dataflow result; a map from each program point to a map containing
 * information from each variables
 *)
type 'a mapping = ('a inout) NodeiMap.t

 (* each node in the CFG will have a inbound environment and an outbout one *)
 and 'a inout = {
   in_env: 'a env;
   out_env: 'a env;
 }
  (* the env type below is polymorphic so one can map different things, 
   * for instance a bool for a liveness analysis.
  *)
 and 'a env = 'a VarMap.t

type 'a transfn = 'a mapping -> Controlflow_pil.nodei -> 'a mapping

(* generic entry point *)
val fixpoint: 
  eq:('a -> 'a -> bool) -> 
  init:'a mapping ->
  trans:'a transfn -> 
  flow:Controlflow_pil.flow -> 
  'a mapping

module VarSet : Set.S with type elt = Pil.dname
module NodeiSet : Set.S with type elt = Controlflow_pil.nodei

(* specific dataflow analysis: reaching analysis *)

val reaching_transfer: 
  gen:VarSet.t NodeiMap.t -> 
  kill:(NodeiSet.t VarMap.t) NodeiMap.t ->
  flow:Controlflow_pil.flow -> 
  NodeiSet.t transfn

val reaching_fixpoint: 
  Controlflow_pil.flow -> NodeiSet.t mapping

(* debugging *)
val display_dflow: 
  Controlflow_pil.flow -> 'a mapping -> ('a -> string) -> unit

val display_reaching_dflow: 
 Controlflow_pil.flow -> NodeiSet.t mapping -> unit

(*x: dataflow_pil.mli *)
(*e: dataflow_pil.mli *)
