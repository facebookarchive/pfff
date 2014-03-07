(*s: view_overlays.mli *)

val draw_searched_rectangles:
  dw:Model2.drawing -> unit

val motion_notify:
  Model2.world -> GdkEvent.Motion.t -> bool

val paint_initial:
  Model2.drawing -> unit
val hook_finish_paint:
  Model2.world -> unit

(*e: view_overlays.mli *)
