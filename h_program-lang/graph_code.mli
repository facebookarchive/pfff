
type node = string * Database_code.entity_kind
type edge = Has | Use

type graph

val root: node

(* similar API to graph.ml *)

(* graph construction *)
val create: unit -> graph

val add_node: node -> graph -> unit
val add_edge: (node * node) -> edge -> graph -> unit

(* graph access *)
val has_node: node -> graph -> bool
(* can raise exception *)
val parent: node -> graph -> node

(* algorithms *)

(* debugging support *)
val display_with_gv: graph -> unit

(* internals *)
