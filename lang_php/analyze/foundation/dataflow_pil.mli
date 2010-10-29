(*s: dataflow_pil.mli *)

(* the goal of a dataflow analysis is to store information about each
 * variable (VarMap) at each program point (NodeiMap).
 *)
module VarMap : Map.S with type key = string

(* The final dataflow result; a map from each program point to a map
 * containing information from each variables.
 *
 * The array is really a Map with nodei as a key. This means the flow
 * must have nodei that starts at 0 and are not too spreaded. This is
 * currently the case when the graph comes from
 * Controlflow_build_pil.ml (itself using Ograph_extended.ml).
 *)
type 'a mapping = ('a inout) array

(* each node in the CFG will have a inbound environment and an outbout one *)
 and 'a inout = {
   in_env: 'a env;
   out_env: 'a env;
 }
  (* the env type below is polymorphic so one can map different things,
   * for instance a bool for a liveness analysis.
  *)
 and 'a env = 'a VarMap.t

type 'a transfn = 'a mapping -> Controlflow_pil.nodei -> 'a inout

(* generic entry point *)
val fixpoint:
  eq:('a -> 'a -> bool) ->
  init:'a mapping ->
  trans:'a transfn ->
  flow:Controlflow_pil.flow ->
  forward: bool ->
  'a mapping

module VarSet : Set.S with type elt = string
module NodeiSet : Set.S with type elt = Controlflow_pil.nodei

(* specific dataflow analysis *)

val reaching_fixpoint:
  Controlflow_pil.flow -> NodeiSet.t mapping

val liveness_fixpoint:
  Controlflow_pil.flow -> unit mapping

(* debugging *)
val display_dflow:
  Controlflow_pil.flow -> 'a mapping -> ('a -> string) -> unit

val display_reaching_dflow:
 Controlflow_pil.flow -> NodeiSet.t mapping -> unit

val display_liveness_dflow:
 Controlflow_pil.flow -> unit mapping -> unit

(* internals *)
val reaching_transfer:
  gen:VarSet.t array ->
  kill:(NodeiSet.t env) array ->
  flow:Controlflow_pil.flow ->
  NodeiSet.t transfn


(*x: dataflow_pil.mli *)
(*e: dataflow_pil.mli *)
