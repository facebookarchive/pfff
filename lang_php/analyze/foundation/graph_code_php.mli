
val add_fake_node_when_undefined_entity: bool ref

val build:
  ?verbose:bool -> 
  ?logfile:Common.filename ->
  ?readable_file_format:bool ->
  ?only_defs:bool -> 
  (Common.dirname, Common.filename list) Common.either ->
  Skip_code.skip list ->
  Graph_code.graph

(* used by scheck *)
type resolved_name = R of string

val lookup_inheritance:
  Graph_code.graph ->
  (resolved_name * string) ->
  'a ->
  ((resolved_name * 'a) * Database_code.entity_kind) option
