
val gen_red_green_layer: 
  Coverage_code.lines_coverage -> 
  output:Common.filename ->
  unit

val gen_heatmap_layer: 
  Coverage_code.lines_coverage -> 
  output:Common.filename ->
  unit

val actions : unit -> Common.cmdline_actions

