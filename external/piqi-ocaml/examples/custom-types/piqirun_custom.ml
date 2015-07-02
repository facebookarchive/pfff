(* 
 * runtime support for OCaml custom types defined in example.piqi
 *)

(* mapping OCaml char to Piqi int *)
type char = Char.t

let char_of_int: int -> char = Char.chr
let char_to_int: char -> int = Char.code


(* mapping OCaml nativeint to Piqi int *)
type nativeint = Nativeint.t

let nativeint_of_int: int -> nativeint = Nativeint.of_int
let nativeint_to_int: nativeint -> int = Nativeint.to_int


(* mapping OCaml big_int to Piqi string (by representing number as a decimal
 * string)
 *)
type bigint = Big_int.big_int

let bigint_of_string: string -> bigint = Big_int.big_int_of_string
let bigint_to_string: bigint -> string = Big_int.string_of_big_int



(*
 * mapping string key-value map (string Map.Make(String)
 * to the list of {key, value} records
 *)


module M = Map.Make(String)
type skvl = string M.t


let list_of_map map =
  let l = M.fold (fun k v accu -> (k,v)::accu) map [] in
  List.rev l

let map_of_list pairs =
  List.fold_left (fun accu (k,v) -> M.add k v accu) M.empty pairs


open Skvl_piqi.String_key_value

let skvl_of_string_key_value_list (x: Skvl_piqi.string_key_value_list) :skvl =
  (*
  M.empty
  *)
  let pairs = List.map (fun x -> x.key, x.value) x in
  map_of_list pairs


let skvl_to_string_key_value_list (x :skvl) :Skvl_piqi.string_key_value_list =
  (*
  Skvl_piqi.default_string_key_value_list ()
  *)
  let pairs = list_of_map x in
  List.map (fun (k, v) -> {key = k; value = v}) pairs

