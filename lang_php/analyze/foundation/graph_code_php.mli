
val add_fake_node_when_undefined_entity: bool ref
val lookup_inheritance:
  Graph_code.graph ->
  (string * string) ->
  'a ->
  ((string * 'a) * Database_code.entity_kind) option
val build:
  ?verbose:bool -> 
  ?logfile:Common.filename ->
  ?readable_file_format:bool ->
  ?only_defs:bool -> 
  (Common.dirname, Common.filename list) Common.either ->
  Skip_code.skip list ->
  Graph_code.graph

