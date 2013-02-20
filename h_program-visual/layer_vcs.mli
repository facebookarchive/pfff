
val gen_age_layer:
  ?verbose:bool ->
  line_granularity: bool ->
  skip_revs:Lib_vcs.versionid list ->
  Common2.path -> output:Common.filename -> unit

val gen_nbauthors_layer: 
  ?verbose:bool ->
  skip_revs:Lib_vcs.versionid list ->
  Common2.path -> output:Common.filename -> unit

val actions : unit -> Common.cmdline_actions
