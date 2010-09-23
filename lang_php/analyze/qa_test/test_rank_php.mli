
(* print statistics on stdout *)
val functions_not_covered_and_high_code_rank: 
  ?topn:int -> 
  is_test_file:(Common.filename -> bool) ->
  Database_php.database -> unit

val actions: unit -> Common.cmdline_actions
