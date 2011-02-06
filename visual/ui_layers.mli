(*s: ui_layers.mli *)
val choose_layer: 
  root:Common.dirname ->
  string option (* layer title we want *) -> 
  Model2.drawing ref -> 
  unit
(*e: ui_layers.mli *)
