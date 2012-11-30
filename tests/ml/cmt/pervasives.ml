
external raise : exn -> 'a = "%raise"

external ( + ) : int -> int -> int = "%addint"

type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

external incr : int ref -> unit = "%incr"
