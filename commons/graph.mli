
(* graph of polymorphic nodes *)
type 'a graph

(* graph construction *)
val create : 
  unit -> 'a graph
val add_vertex_if_not_present : 
  'a -> 'a graph -> unit
val add_edge : 
  'a -> 'a -> 'a graph -> unit

(* algorithms *)
val shortest_path: 
  'a -> 'a -> 'a graph -> 'a list
val transitive_closure:
  'a graph -> 'a graph
val strongly_connected_components:
  'a graph -> ('a list array * ('a, int) Hashtbl.t)
val strongly_connected_components_condensation:
  'a graph -> ('a list) graph

(* debugging support *)
val print_graph_generic :
  str_of_key:('a -> string) -> Common.filename -> 'a graph -> unit

val display_with_gv:
  'a graph -> unit
val display_strongly_connected_components :
  str_of_key:('a -> string) -> ('a, int) Hashtbl.t -> 'a graph -> unit


(* internals *)

(*
val vertex_of_key : 'a -> 'a graph -> OG.V.t
val key_of_vertex : OG.V.t -> 'a graph -> 'a
*)
