
val invoke : ('a -> 'b) -> 'a -> unit -> 'b

val parallel_map : ('a -> 'b) -> 'a list -> 'b list

type 'a job = unit -> 'a
type 'a jobs = ('a job) list

val map_jobs: tasks:int -> 'a jobs -> 'a list
