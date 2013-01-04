
type node = string * Database_code.entity_kind
type edge = Has | Use
type nodeinfo = {
  pos: Parse_info.parse_info;
  props: property list;
}
 and property =
   | IsEnum

type error =
 | NodeAlreadyPresent

exception Error of error

(* really an hypergraph actually *)
type graph

val save: graph -> Common.filename -> unit
val load: Common.filename -> graph

val root: node
 val pb: node
  val not_found: node
  val dupe: node
(* val stdlib: node *)

val create_initial_hierarchy: graph -> unit

(* similar API to graph.ml *)

(* graph construction *)
val create: unit -> graph
(* may raise NodeAlreadyPresent *)
val add_node: node -> graph -> unit
val add_nodeinfo: node -> nodeinfo -> graph -> unit
val add_edge: (node * node) -> edge -> graph -> unit
val create_intermediate_directories_if_not_present: 
  graph -> Common.dirname -> unit
val remove_edge: (node * node) -> edge -> graph -> unit

(* graph access *)
val has_node: node -> graph -> bool
val succ: node -> edge -> graph -> node list
val pred: node -> edge -> graph -> node list
(* can raise exception *)
val parent: node -> graph -> node
val parents: node -> graph -> node list
val children: node -> graph -> node list
val nodeinfo: node -> graph -> nodeinfo

val all_children: node -> graph -> node list

val iter_use_edges: (node -> node -> unit) -> graph -> unit
val iter_nodes: (node -> unit) -> graph -> unit
val all_use_edges: graph -> (node * node) list

val nb_nodes: graph -> int
val nb_use_edges: graph -> int

(* algorithms *)

(* debugging support *)
val string_of_node: node -> string
val display_with_gv: graph -> unit

(* internals *)
