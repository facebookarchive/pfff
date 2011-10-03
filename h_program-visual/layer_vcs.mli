
val gen_age_layer:
  line_granularity: bool ->
  skip_revs:Lib_vcs.versionid list ->
  Common.path -> output:Common.filename -> unit

val gen_nbauthors_layer: 
  skip_revs:Lib_vcs.versionid list ->
  Common.path -> output:Common.filename -> unit

val actions : unit -> Common.cmdline_actions
