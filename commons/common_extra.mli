
val execute_and_show_progress:
  show_progress:bool -> int -> ((unit -> unit) -> 'a) -> unit

val execute_and_show_progress2:
  show_progress:bool -> int -> ((unit -> unit) -> 'a) -> 'a

val set_link: unit -> unit
