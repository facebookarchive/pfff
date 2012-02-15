
val execute_and_show_progress:
  show:bool -> int -> ((unit -> unit) -> 'a) -> unit

val execute_and_show_progress2:
  ?show:bool -> int -> ((unit -> unit) -> 'a) -> 'a

val with_progress_list_metter:
  ?show:bool -> ((unit -> unit) -> 'b) -> 'a list -> 'b

val progress:
  ?show:bool -> ((unit -> unit) -> 'b) -> 'a list -> 'b

val set_link: unit -> unit
