(*s: tainted_php.mli *)

type tainted = bool

val tainted_analysis: 
  Controlflow_php.flow -> tainted Dataflow_php.mapping

val check_bad_echo: 
  Controlflow_php.flow -> tainted Dataflow_php.mapping -> unit

val display_tainted_flow: 
  Controlflow_php.flow -> tainted Dataflow_php.mapping -> unit

(*e: tainted_php.mli *)
