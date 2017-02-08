
val graph_print: Graph_code.graph -> string -> unit
val edge_string: Graph_code.edge -> string 
val entity_kind_piqi: Entity_code.entity_kind -> Graph_code_piqi.entity_kind
val token_location_piqi: Parse_info.token_location ->
  Graph_code_piqi.token_location 
val property_list_piqi: Entity_code.property list ->
  Graph_code_piqi.property_list 
