(*s: coverage_static_php.mli *)

val mk_is_covered_by_test: 
  is_test_file:(Common.filename -> bool) ->
  Database_php.database -> (Entity_php.id -> bool)

(*e: coverage_static_php.mli *)
