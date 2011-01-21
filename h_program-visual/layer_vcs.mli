
val gen_age_layer:
  line_granularity: bool ->
  Common.path -> output:Common.filename -> unit

val gen_nbauthors_layer: 
  Common.path -> output:Common.filename -> unit

val actions : unit -> Common.cmdline_actions
