open Pervasives

let global = ref 0

let use_global () =
  let global = 
    match !global with
    | 0 -> 1
    | _ -> 2
  in
  global
