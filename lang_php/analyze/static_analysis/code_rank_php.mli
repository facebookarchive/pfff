(*s: code_rank_php.mli *)

type code_ranks = {
  function_ranks: (Database_php.id, float) Oassoc.oassoc;
}


val build_code_ranks: 
  Database_php.database -> code_ranks

val build_naive_caller_ranks: 
  Database_php.database -> code_ranks
(*x: code_rank_php.mli *)
(*e: code_rank_php.mli *)
