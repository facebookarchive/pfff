(*s: graph_php.mli *)

type id_graph = Entity_php.id Graph.graph

(* can be used for regular callgraph but also for use of entities like class *)
val build_entities_graph: 
  all_ids: Entity_php.id list ->
  successors: (Entity_php.id -> Entity_php.id list) ->
  id_graph

(*x: graph_php.mli *)
(*e: graph_php.mli *)
