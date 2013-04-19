
type path = string list

val build: 
  Graph_code_opti.graph ref -> path -> Dependencies_matrix_code.dm
