
type graph = (string * Common2.StringSet.t) list

val emptyGraph : graph

val divideGraph : graph -> string -> Common2.StringSet.t * graph

val addEdge :  graph -> string -> string -> graph
val addEdges :  graph -> string -> Common2.StringSet.t -> graph
val removeEdge :  graph -> string -> string -> graph
val removeEdges :  graph -> string -> Common2.StringSet.t -> graph

val edgesOfGraph : graph -> (string * string) list
val graphOfEdges : (string * string) list -> graph

val isEdge : graph -> string -> string -> bool

val targetsOf : graph -> string -> Common2.StringSet.t
val sourcesOf : graph -> string -> string list

val addEdgeTc :  graph ->  string -> string -> graph
val tc :  graph -> graph
val addEdgeTk :  graph ->  graph ->  string ->  string ->  graph * graph

(* transitive reduction of a graph *)
val tk :  graph -> graph

val printGraph : graph -> unit
