
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * PHP serialization 
 * http://php.net/manual/en/function.serialize.php
 *
 * src: found on the web at ????
 * I just un-extlib-ized it.
 * 
 * Anatomy of a serialize()'ed value:
 * 
 * String
 * s:size:value;
 * 
 * Integer
 * i:value;
 * 
 * Boolean
 * b:value; (does not store "true" or "false", does store '1' or '0')
 * 
 * Null
 * N;
 * 
 * Array
 * a:size:{key definition;value definition;(repeated per element)}
 * 
 * Object
 * O:strlen(object name):object name:object size:{s:strlen(property name):property name:property definition;(repeated per property)}
 * 
 * String values are always in double quotes
 * Array keys are always integers or strings
 * "null => 'value'" equates to 's:0:"";s:5:"value";',
 * "true => 'value'" equates to 'i:1;s:5:"value";',
 * "false => 'value'" equates to 'i:0;s:5:"value";',
 * "array(whatever the contents) => 'value'" equates to an "illegal offset type" warning because you can't use an
 * array as a key; however, if you use a variable containing an array as a key, it will equate to 's:5:"Array";s:5:"value";',
 * and
 * attempting to use an object as a key will result in the same behavior as using an array will.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type php = 
  | AI of (int * php) list 
  | AS of (string * php) list 
  | S of string 
  | I of int 
  | B of bool 
  | F of float 
  | N
 (* with tarzan *)

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

let rec vof_php =
  function
  | AI v1 ->
      let v1 =
        Ocaml.vof_list
          (fun (v1, v2) ->
             let v1 = Ocaml.vof_int v1
             and v2 = vof_php v2
             in Ocaml.VTuple [ v1; v2 ])
          v1
      in Ocaml.VSum (("AI", [ v1 ]))
  | AS v1 ->
      let v1 =
        Ocaml.vof_list
          (fun (v1, v2) ->
             let v1 = Ocaml.vof_string v1
             and v2 = vof_php v2
             in Ocaml.VTuple [ v1; v2 ])
          v1
      in Ocaml.VSum (("AS", [ v1 ]))
  | S v1 -> let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("S", [ v1 ]))
  | I v1 -> let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("I", [ v1 ]))
  | B v1 -> let v1 = Ocaml.vof_bool v1 in Ocaml.VSum (("B", [ v1 ]))
  | F v1 -> let v1 = Ocaml.vof_float v1 in Ocaml.VSum (("F", [ v1 ]))
  | N -> Ocaml.VSum (("N", []))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* open Prelude *)
let (>>) x f = f x
let ($) f g = function x -> f (g x)

(* pad: from extString.ml *)
let string_init len f =
  let s = String.create len in
  for i = 0 to len - 1 do
    String.unsafe_set s i (f i)
  done;
  s


let check x y = 
  if x <> y 
  then failwith (Printf.sprintf "Php_serialize failed : %u <> %u" x y)

(*****************************************************************************)
(* Serialized string -> php *)
(*****************************************************************************)

let rec parse_one = parser
  | [< ''a'; '':'; n=number; '':'; ''{'; a=parse_array; ''}' >] -> ignore n;(*check n (List.length a);*) a
  | [< ''b'; '':'; n=number; '';' >] -> B (0 <> n)
  | [< ''d'; '':'; f=parse_float_semi; >] -> F f
  | [< n=parse_int >] -> I n
  | [< s=parse_str >] -> S s
  | [< ''N'; '';' >] -> N
and number t = parse_nat 0 t
and parse_nat n = parser (* overflow test?* *)
  | [< ''0'..'9' as c; t >] -> let digit = Char.code c - Char.code '0' in parse_nat (n * 10 + digit) t
  | [< >] -> n
and integer = parser
  | [< ''-'; t >] -> - (number t)
  | [< t >] -> number t
and parse_int = parser
  | [< ''i'; '':'; n=integer; '';' >] -> n
and parse_float_semi t = (* ugly, because of one look ahead token FIXME *)
  let buf = Scanf.Scanning.from_function (fun () -> Stream.next t) in
  Scanf.bscanf buf "%f;" (fun f -> f)
and parse_str = parser
  | [< ''s'; '':'; n=number; '':'; ''"'; s=take_string n; ''"'; '';' >] -> s
and take_string n t = string_init n (fun _ -> Stream.next t)
and parse_array = parser
  | [< k=parse_int; v=parse_one; a=parse_int_array [k,v] >] -> AI a
  | [< k=parse_str; v=parse_one; a=parse_str_array [k,v] >] -> AS a
  | [< >] -> AI [] (* empty array *)
and parse_int_array acc = parser
  | [< k=parse_int; v=parse_one; t >] -> parse_int_array ((k,v)::acc) t
  | [< >] -> List.rev acc
and parse_str_array acc = parser
  | [< k=parse_str; v=parse_one; t >] -> parse_str_array ((k,v)::acc) t
  | [< >] -> List.rev acc

let parse stream =
  let show () =
    let tail = Stream.npeek 10 stream >> List.map (String.make 1) >> String.concat "" in
    Printf.sprintf "Position %u : %s" (Stream.count stream) tail
  in
  try
    let r = parse_one stream in
    Stream.empty stream; r
  with
  | Stream.Error _ | Stream.Failure -> failwith (show ())

let parse_string = parse $ Stream.of_string

(*****************************************************************************)
(* Combinators for easy deconstruction *)
(*****************************************************************************)

exception Error of string

let fail v str = 
  raise (Error (Printf.sprintf "%s : %s" str (Common.dump v)))

let int = function I n -> n | x -> fail x "int"
let str = function S s -> s | x -> fail x "str"

let opt k x = try Some (k x) with Error _ -> None

let values f = function
  | AS a -> List.map (f $ snd) a
  | AI a -> List.map (f $ snd) a
  | x -> fail x "values"

let array f = function
  | AS a -> List.map (fun (k,v) -> k, f v) a
  | x -> fail x "array"

let assoc php name =
  match php with
  | AS a -> List.assoc name a
  | _ -> fail php "assoc"


(*****************************************************************************)
(* php -> Serialized string *)
(*****************************************************************************)

module Out = struct

(** Combinators to build values of [php] type *)

let str s = S s
let int n = I n

(* pad: from extList.ml *)
type 'a mut_list =  {
	hd: 'a; 
	mutable tl: 'a list
}
let dummy_node () = { hd = Obj.magic (); tl = [] }
external inj : 'a mut_list -> 'a list = "%identity"
let list_of_enum e =
	let h = dummy_node() in
	let _ = Enum.fold (fun x acc ->
		let r = { hd = x; tl = [] }  in
		acc.tl <- inj r;
		r) h e in
	h.tl



let  array f e = AI (e >> Enum.mapi (fun i x  -> i, f x) >> list_of_enum)
let iarray f e = AI (e >> Enum.map (fun (k,v) -> k, f v) >> list_of_enum)
let sarray f e = AS (e >> Enum.map (fun (k,v) -> k, f v) >> list_of_enum)

(** Serialize [php] value *)

(* pad:from IO.ml *)
type 'a output = {
	mutable out_write : char -> unit;
	mutable out_output : string -> int -> int -> int;
	mutable out_close : unit -> 'a;
	mutable out_flush : unit -> unit;
}
let output o s p l =
	let sl = String.length s in
	if p + l > sl || p < 0 || l < 0 then invalid_arg "IO.output";
	o.out_output s p l
let nwrite o s =
	let p = ref 0 in
	let l = ref (String.length s) in
	while !l > 0 do
		let w = o.out_output s !p !l in
		if w = 0 then raise Sys_blocked_io;
		p := !p + w;
		l := !l - w;
	done
let io_write o x = o.out_write x
let io_printf o fmt =
	Printf.kprintf (fun s -> nwrite o s) fmt
let io_output_string () =
  let b = Buffer.create 0 in
  {
    out_write = (fun c ->
      Buffer.add_char b c
    );
    out_output = (fun s p l ->
      Buffer.add_substring b s p l;
      l
    );
    out_close = (fun () -> Buffer.contents b);
    out_flush = (fun () -> ());
  }
exception Output_closed
let io_close_out o =
	let f _ = raise Output_closed in
	let r = o.out_close() in
	o.out_write <- f;
	o.out_output <- f;
	o.out_close <- f;
	o.out_flush <- f;
	r



let output out v =
  let put_arr f a = 
    io_printf out "a:%u:{" (List.length a); List.iter f a; 
    io_write out '}' in
  let rec put = function
    | AS a -> put_arr (fun (k,v) -> put (S k); put v) a
    | AI a -> put_arr (fun (k,v) -> put (I k); put v) a
    | I n -> io_printf out "i:%i;" n
    | B b -> io_printf out "b:%u;" (if b then 1 else 0)
    | F f -> io_printf out "d:%f;" f
    | N -> nwrite out "N;"
    | S s -> io_printf out "s:%u:\"%s\";" (String.length s) s
  in
  put v

end
(*
let to_string v =
  let out = Out.io_output_string () in
  Out.output out v;
  Out.io_close_out o ???
*)
