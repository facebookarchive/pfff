
val _set_title:         (string -> unit) ref
val _statusbar_addtext: (string -> unit) ref
val _label_settext: (string -> unit) ref

val _refresh_drawing_area: (unit -> unit) ref

val current_motion_refresher:
  GMain.Idle.id option ref
