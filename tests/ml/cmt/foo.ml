open Pervasives

let constant = 1
let func x y = x + y

let list1_call_qualified = List.map (fun x -> x) [1;2]

open List
let list2_call_unqualified = map (fun x -> x) [3;4]

let global = ref 0
let hglobal = Hashtbl.create 101

let use_global () = 
  incr global

let use_hglobal () = 
  Hashtbl.add hglobal 1 true


let use_ref = function
 { contents = x} -> x

exception Error1

let raise_exn () =
  try 
    raise Error1
  with Error1 -> ()

let raise_exn2 () =
  try 
    raise Bar.BarExn
  with Bar.BarExn -> ()

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

let use_constructor x =
  match x with
  | A -> 1
  | B -> 2

let use_constructor_nested_module x =
  match x with
  | C -> 1
  | D -> 2


open X.Z
let use_field_nested_module x = 
  x.fld1

type another_int = X.t

let (func_with_signature: another_int -> bool) = fun x -> true

let partial_application x = func x
