
val build:
  ?verbose:bool -> 
  root:Common.path ->
  cmt_files:Common.filename list -> 
  ml_files:Common.filename list ->
  Graph_code.graph
