
val gen_red_green_layer:
  root:Common.dirname ->
  output:Common.filename ->
  Statistics_parsing.parsing_stat list -> 
  unit

val gen_heatmap_layer:
  root:Common.dirname ->
  output:Common.filename ->
  Statistics_parsing.parsing_stat list -> 
  unit
