
val build:
  ?verbose:bool -> ?only_defs:bool ->
  (Common.dirname, Common.filename list) Common.either ->
  Skip_code.skip list ->
  Graph_code.graph

(* internal *)

