(*s: cyclomatic_php.mli *)
val cyclomatic_complexity_func: 
  ?verbose:bool ->
  Ast_php.func_def -> int

(* internal *)
val cyclomatic_complexity_flow: 
  ?verbose:bool ->
  Controlflow_php.flow -> int
(*e: cyclomatic_php.mli *)
