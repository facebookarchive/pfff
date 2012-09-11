
type node = string * Database_code.entity_kind
type edge = Has | Use

(* really an hypergraph actually *)
type graph

val save: graph -> Common.filename -> unit
val load: Common.filename -> graph

val root: node

(* similar API to graph.ml *)

(* graph construction *)
val create: unit -> graph
val add_node: node -> graph -> unit
val add_edge: (node * node) -> edge -> graph -> unit
val create_intermediate_directories_if_not_present: 
  graph -> Common.dirname -> unit

(* graph access *)
val has_node: node -> graph -> bool
val succ: node -> edge -> graph -> node list
val pred: node -> edge -> graph -> node list
(* can raise exception *)
val parent: node -> graph -> node
val parents: node -> graph -> node list

val iter_use_edges: (node -> node -> unit) -> graph -> unit
val iter_nodes: (node -> unit) -> graph -> unit

val nb_nodes: graph -> int
val nb_use_edges: graph -> int

(* algorithms *)

(* debugging support *)
val string_of_node: node -> string
val display_with_gv: graph -> unit

(* internals *)
