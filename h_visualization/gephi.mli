
val graph_to_gefx: 
  str_of_node:('a -> string) ->
  output:Common.filename ->
  tree:('a, 'a) Common.tree option ->
  'a Graph.graph ->
  unit
