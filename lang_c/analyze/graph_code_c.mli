
val build:
  ?verbose:bool -> Common.dirname -> Common.filename list ->
  Graph_code.graph

(* if want prolog fact generation *)
val hook_use_edge: 
  (Graph_code_prolog.context -> bool -> 
  (Graph_code.node * Graph_code.node) -> 
   Graph_code.graph -> 
   unit)
  ref

(* if want datalog fact generation *)
val facts: Datalog_c.fact list ref option ref
