open List

let a = 1
let b x y = x + y

let c = List.map (fun x -> x) [1;2]
let d = map (fun x -> x) [3;4]

module X = struct
  type t = A | B
  module Y = struct
      type t = C | D
  end
  module Z = struct
      type t = { fld1: int; fld2: int }
  end
end

open X
open X.Y

let e x =
  match x with
  | A -> 1
  | B -> 2

let f x =
  match x with
  | C -> 1
  | D -> 2


open X.Z
let g x = 
  x.fld1

type another_int = X.t

let (h: another_int -> bool) = fun x -> true

let i x = b x
