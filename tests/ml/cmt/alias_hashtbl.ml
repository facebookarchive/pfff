type ('a,'b) hash = ('a,'b) Hashtbl.t

type foo = {
  fld1: (int, int) hash;
}

let empty_hash = Hashtbl.create 0

(* TODO *)
let test x = 
  if true then x.fld1 else empty_hash
