
val gen_layer: 
  xhprof_file:Common.filename ->
  db:Database_php.database -> 
  output:Common.filename ->
  unit

val actions : unit -> Common.cmdline_actions
