
val execute_and_show_progress:
  show_progress:bool -> int -> ((unit -> unit) -> 'a) -> unit

val execute_and_show_progress2:
  ?show_progress:bool -> int -> ((unit -> unit) -> 'a) -> 'a

val with_progress_list_metter:
  ?show_progress:bool -> ((unit -> unit) -> 'a list -> 'b) -> 'a list -> 'b

val set_link: unit -> unit
