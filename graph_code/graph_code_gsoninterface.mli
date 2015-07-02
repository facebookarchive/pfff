open Graph_code

val add_node: node -> graph -> unit
val add_nodeinfo: node -> nodeinfo -> graph -> unit
val add_edge: (node * node) -> edge -> graph -> unit
val add_edgeinfo: (node * node) -> edge -> edgeinfo -> graph -> unit

val create_initial_hierarchy: graph -> unit
val create_intermediate_directories_if_not_present: 
  graph -> Common.dirname -> unit

val graph_of_dotfile: Common.filename -> graph

(* Links to Graph_print_options.merge_graphson *)
val merge_graphson: unit -> unit

