(*s: cyclomatic_php.mli *)
(* core service *)
val cyclomatic_complexity_func: 
  ?verbose:bool ->
  Ast_php.func_def -> int

val cyclomatic_complexity_file:
  Common.filename -> (Ast_php.name * int) list

(* internal *)
val cyclomatic_complexity_flow: 
  ?verbose:bool ->
  Controlflow_php.flow -> int

(* other services *)
type selection = 
  | Threshold of int
  | Topn of int

val code_with_bad_cyclomatic:
  selection -> Common.filename list -> unit
(*e: cyclomatic_php.mli *)
