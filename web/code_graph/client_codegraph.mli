
val paint: 
  Model_codegraph.world_client -> 
  (string, unit) Eliom_pervasives.server_function ->
  (unit, string) Eliom_pervasives.server_function ->
  (int * int, string) Eliom_pervasives.server_function ->
  unit Lwt.t
