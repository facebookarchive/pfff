(*s: view_mainmap.mli *)

val paint: Model2.drawing -> Model2.model Async.t -> unit

val zoom_pan_scale_map: Cairo.t -> Model2.drawing -> unit

val device_to_user_area: Model2.drawing -> Figures.rectangle

val with_map: Model2.drawing -> (Cairo.t -> 'a) -> 'a

val button_action:
   Model2.world -> GdkEvent.Button.t -> bool

(*e: view_mainmap.mli *)
