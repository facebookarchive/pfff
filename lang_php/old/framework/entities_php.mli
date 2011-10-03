(*s: entities_php.mli *)

type idtree = Callgraph_php.idtree

val tree_of_ids: ?sort:bool -> 
  Entity_php.id list -> (Entity_php.id -> string) -> idtree

val first_id_in_tree: idtree -> Entity_php.id


(*x: entities_php.mli *)
(*e: entities_php.mli *)
