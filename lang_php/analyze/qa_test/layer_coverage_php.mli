
val gen_red_green_layer: 
  Coverage_tests_php.lines_coverage -> 
  output:Common.filename ->
  unit

val gen_heatmap_layer: 
  Coverage_tests_php.lines_coverage -> 
  output:Common.filename ->
  unit

val actions : unit -> Common.cmdline_actions
