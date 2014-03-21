(*s: async.mli *)

(*s: type async *)
type 'a t = {
  m: Mutex.t; 
  c: Condition.t;
  v: 'a option ref;
  }
(*e: type async *)
(*s: async functions sig *)
val async_make: unit -> 'a t
val async_get: 'a t -> 'a
val async_set: 'a -> 'a t -> unit
val async_ready: 'a t -> bool
val async_get_opt: 'a t -> 'a option

val with_lock: (unit -> 'a) -> Mutex.t -> 'a
(*e: async functions sig *)

(*e: async.mli *)
