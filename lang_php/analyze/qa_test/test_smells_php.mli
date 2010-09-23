
type selection = 
  | Threshold of int
  | Topn of int

val code_with_bad_cyclomatic: 
  selection -> Common.filename list -> unit


val actions: unit -> Common.cmdline_actions
