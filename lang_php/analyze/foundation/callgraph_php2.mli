
type node =
  | Function of string
  | Method of string * string
  | File of Common.filename
  (* used to simplify code to provoke the call to toplevel functions *)
  | FakeRoot

type callgraph = (node, node Set_poly.t) Map_poly.t

val add_graph: node -> node -> callgraph -> callgraph
(* used as reduce step for the map-reduce computing the callgraph *)
val union_graph: callgraph -> callgraph -> callgraph

val string_of_node: node -> string
val node_of_string: string -> node
