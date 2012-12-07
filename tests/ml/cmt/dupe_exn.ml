(* this is quite common in ocaml compiler source code, unfortunatly *)

exception Non_closed

let f () = 
  Pervasives.raise Non_closed

exception Non_closed of string

let f2 () = 
  Pervasives.raise (Non_closed "foo")



