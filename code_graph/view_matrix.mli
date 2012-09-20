
val paint: Model3.world -> unit

val button_action:
 < as_widget : [> `widget ] Gtk.obj; .. > ->
   Model3.world -> GdkEvent.Button.t -> bool

val recompute_matrix:
  Model3.world -> unit
