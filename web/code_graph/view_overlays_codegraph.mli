
val mousemove:
  Canvas_helpers.context ->
  Model_codegraph.world_client ->
  (Dom_html.element Js.t -> Dom_html.mouseEvent Js.t 
   -> unit)
