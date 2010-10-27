(*s: dataflow_pil.mli *)

module DM : Map.S with type key = Pil.dname
module DS : Set.S with type elt = Pil.dname
type ni = Controlflow_pil.nodei

module NS : Set.S with type elt = ni
module NM : Map.S with type key = ni

type 'a env = 'a DM.t

type 'a inout = {
  in_env: 'a env;
  out_env: 'a env;
}

type 'a mapping = ('a inout) NM.t

val fixpoint: Controlflow_pil.flow -> ('a -> 'a -> bool) -> 'a mapping ->
  ('a mapping -> Ograph_extended.nodei -> 'a mapping) -> 'a mapping

val reaching_transfer: Controlflow_pil.flow -> DS.t NM.t -> (NS.t DM.t) NM.t ->
    NS.t mapping -> ni -> NS.t mapping
val reaching_fixpoint: Controlflow_pil.flow -> NS.t mapping

val display_dflow: Controlflow_pil.flow -> 'a mapping -> ('a -> string) -> unit
val display_reaching_dflow: Controlflow_pil.flow -> NS.t mapping -> unit

(*x: dataflow_pil.mli *)
(*e: dataflow_pil.mli *)
