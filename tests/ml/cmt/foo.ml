open List

let a = 1
let b x y = x + y

let c = List.map (fun x -> x) [1;2]
let d = map (fun x -> x) [3;4]

module X = struct
  type t = A | B
end

open X

let e x =
  match x with
  | A -> 1
  | B -> 2

type another_int = X.t

let (g: another_int -> bool) = fun x -> true

let h x = b x
