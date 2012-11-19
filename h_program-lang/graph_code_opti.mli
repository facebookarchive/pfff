
type graph = {
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: Graph_code.node array;

  has_children: (int list) array;

  (* use the multi key property of the hash *)
  use: (int list) array;
}

val convert: Graph_code.graph -> graph

val nb_nodes: graph -> int
