
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

(* debugging support *)
val print_graph_generic :
  str_of_key:('a -> string) -> Common.filename -> 'a graph -> unit
val display_with_gv:
  'a graph -> unit








(* internals *)

(*
val vertex_of_key : 'a -> 'a graph -> OG.V.t
val key_of_vertex : OG.V.t -> 'a graph -> 'a
*)
