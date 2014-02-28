open Printf
open Lexing

type json_type =
  | Object of (string * json_type) list
  | Array of json_type list

  | String of string
  | Int of int
  | Float of float
  | Bool of bool

  | Null

type t = json_type

exception Json_error of string

let json_error s = raise (Json_error s)


module Browse =
struct

  let make_table l = 
    let tbl = Hashtbl.create (List.length l) in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) l;
    tbl

  let field tbl x =
    match Hashtbl.find_all tbl x with
	[y] -> y
      | [] -> json_error ("Missing field " ^ x)
      | _ -> json_error ("Only one field " ^ x ^ " is expected")

  let fieldx tbl x =
    match Hashtbl.find_all tbl x with
	[y] -> y
      | [] -> Null
      | _ -> json_error ("At most one field " ^ x ^ " is expected")

  let optfield tbl x =
    match Hashtbl.find_all tbl x with
	[y] -> Some y
      | [] -> None
      | _ -> json_error ("At most one field " ^ x ^ " is expected")

  let optfieldx tbl x =
    match Hashtbl.find_all tbl x with
	[y] -> 
	  if y = Null then None
	  else Some y
      | [] -> None
      | _ -> json_error ("At most one field " ^ x ^ " is expected")

  let describe = function
      Bool true -> "true"
    | Bool false -> "false"
    | Int i -> string_of_int i
    | Float x -> string_of_float x
    | String s -> sprintf "%S" s
    | Object _ -> "an object"
    | Array _ -> "an array"
    | Null -> "null"

  let type_mismatch expected x =
    let descr = describe x in
    json_error (sprintf "Expecting %s, not %s" expected descr)

  let is_null x = x = Null
  let is_defined x = x <> Null

  let null = function
      Null -> ()
    | x -> type_mismatch "a null value" x

  let string = function
      String s -> s
    | x -> type_mismatch "a string" x

  let bool = function
      Bool x -> x
    | x -> type_mismatch "a bool" x
	
  let number = function
      Float x -> x
    | Int i -> Pervasives.float i
    | x -> type_mismatch "a number" x
	
  let int = function
      Int x -> x
    | x -> type_mismatch "an int" x

  let float = function
      Float x -> x
    | x -> type_mismatch "a float" x

  let array = function
      Array x -> x
    | x -> type_mismatch "an array" x

  let objekt = function
      Object x -> x
    | x -> type_mismatch "an object" x

  let list f x = List.map f (array x)

  let option = function
      Null -> None
    | x -> Some x

  let optional f = function
      Null -> None
    | x -> Some (f x)

  let assert_object_or_array x =
    match x with
	Object _ 
      | Array _ -> ()
      | _ -> type_mismatch "an array or an object" x
end

module Build =
struct
  let null = Null
  let bool x = Bool x
  let int x = Int x
  let float x = Float x
  let string x = String x
  let objekt l = Object l
  let array l = Array l

  let list f l = Array (List.map f l)

  let option = function
      None -> Null
    | Some x -> x

  let optional f = function
      None -> Null
    | Some x -> f x
end

(* pad: *)
let json_of_list of_a xs = Array(List.map of_a xs)

let string_of_loc (pos1, pos2) =
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)
