
val mouseclick:
  Canvas_helpers.context ->
  Model_codegraph.world_client ->
  (int * int, string) Eliom_pervasives.server_function ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t)
