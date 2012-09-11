
val build:
  ?verbose:bool -> Common.dirname -> Skip_code.skip list ->
  Graph_code.graph

(* internal *)

(* filter noisy modules, test files in external/, etc *)
val filter_ml_files: 
  Common.filename list -> Common.filename list

