(*s: graph_php.mli *)
(*x: graph_php.mli *)

type id_graph = Entity_php.id Graph.graph


(* for debugging simple analysis; only direct function calls are included *)
val build_simple_callgraph:
  Database_php.database -> id_graph

(* the database contains the name of the ids *)
val display_with_gv: 
  id_graph -> Database_php.database -> unit

(* can be used for regular callgraph but also for use of entities like class *)
val build_entities_graph: 
  all_ids: Entity_php.id list ->
  successors: (Entity_php.id -> Entity_php.id list) ->
  id_graph

(*e: graph_php.mli *)
