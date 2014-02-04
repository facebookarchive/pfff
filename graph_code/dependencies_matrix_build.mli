
(* pack less relevant directories under an intermediate "..." subdirectory *)
val threshold_pack: int ref

(* we now return also a new graph because we may have modified the
 * input graph to add some extra "..." nodes
 *)
val build:
  Dependencies_matrix_code.config -> 
  Dependencies_matrix_code.partition_constraints option -> 
  Graph_code_opti.graph -> 
  Dependencies_matrix_code.dm * Graph_code_opti.graph

val info_orders:
 Dependencies_matrix_code.dm -> unit

(* we return a gopti because of threshold_pack that may alter the graph *)
val config_of_path: 
  Dependencies_matrix_code.config_path -> 
  Graph_code_opti.graph ->
  Dependencies_matrix_code.config * Graph_code_opti.graph
