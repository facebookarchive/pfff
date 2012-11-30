open Pervasives

exception Error1

let raise_exn () =
  try 
    raise Error1
  with Error1 -> ()

let raise_exn2 () =
  try 
    raise Bar.BarExn
  with Bar.BarExn -> ()

