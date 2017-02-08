(*
   Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open Piqirun


(* superposition operator *)
let ( ** ) f g x = f (g x)


let assert_eq a b = assert (a = b)


let test_key x =
  let test_parse x =
    (IBuf.of_string ** OBuf.to_string) x
  in
  assert (x = ((fun x -> let _, code = parse_field_header x in code) ** test_parse ** gen_key 0) x)


let test_gen_parse x =
  (init_from_string ** to_string) x


(*
 * Names of the test_* functions correspond to Piqi types: there are 12
 * different integer types
 *)

let test_int x =
  assert (x = (int_of_zigzag_varint ** test_gen_parse ** int_to_zigzag_varint (-1)) x)

let test_int32 x =
  assert (x = (int32_of_zigzag_varint ** test_gen_parse ** int32_to_zigzag_varint (-1)) x)

let test_int64 x =
  assert (x = (int64_of_zigzag_varint ** test_gen_parse ** int64_to_zigzag_varint (-1)) x)


let test_fixed_int32 x =
  assert (x = (int32_of_signed_fixed32 ** test_gen_parse ** int32_to_signed_fixed32 (-1)) x)

let test_fixed_int64 x =
  assert (x = (int64_of_signed_fixed64 ** test_gen_parse ** int64_to_signed_fixed64 (-1)) x)


let test_proto_int x = (* NOTE: this type is not currently defined in Piqi *)
  assert (x = (int_of_signed_varint ** test_gen_parse ** int_to_signed_varint (-1)) x)

let test_proto_int32 x =
  assert (x = (int32_of_signed_varint ** test_gen_parse ** int32_to_signed_varint (-1)) x)

let test_proto_int64 x =
  assert (x = (int64_of_signed_varint ** test_gen_parse ** int64_to_signed_varint (-1)) x)


let test_uint x =
  assert (x = (int_of_varint ** test_gen_parse ** int_to_varint (-1)) x)

let test_uint32 x =
  assert (x = (int32_of_varint ** test_gen_parse ** int32_to_varint (-1)) x)

let test_uint64 x =
  assert (x = (int64_of_varint ** test_gen_parse ** int64_to_varint (-1)) x)


let test_fixed_uint32 x =
  assert (x = (int32_of_fixed32 ** test_gen_parse ** int32_to_fixed32 (-1)) x)

let test_fixed_uint64 x =
  assert (x = (int64_of_fixed64 ** test_gen_parse ** int64_to_fixed64 (-1)) x)


(*
 * Testing packed
 *)

let test_packed_gen_parse x =
  (IBuf.of_string ** OBuf.to_string) x


(*
 * Names of the test_* functions correspond to Piqi types: there are 12
 * different integer types
 *)

let test_packed_int x =
  assert (x = (int_of_packed_zigzag_varint ** test_packed_gen_parse ** int_to_packed_zigzag_varint) x)

let test_packed_int32 x =
  assert (x = (int32_of_packed_zigzag_varint ** test_packed_gen_parse ** int32_to_packed_zigzag_varint) x)

let test_packed_int64 x =
  assert (x = (int64_of_packed_zigzag_varint ** test_packed_gen_parse ** int64_to_packed_zigzag_varint) x)


let test_packed_fixed_int32 x =
  assert (x = (int32_of_packed_signed_fixed32 ** test_packed_gen_parse ** int32_to_packed_signed_fixed32) x)

let test_packed_fixed_int64 x =
  assert (x = (int64_of_packed_signed_fixed64 ** test_packed_gen_parse ** int64_to_packed_signed_fixed64) x)


let test_packed_proto_int x = (* NOTE: this type is not currently defined in Piqi *)
  assert (x = (int_of_packed_signed_varint ** test_packed_gen_parse ** int_to_packed_signed_varint) x)

let test_packed_proto_int32 x =
  assert (x = (int32_of_packed_signed_varint ** test_packed_gen_parse ** int32_to_packed_signed_varint) x)

let test_packed_proto_int64 x =
  assert (x = (int64_of_packed_signed_varint ** test_packed_gen_parse ** int64_to_packed_signed_varint) x)


let test_packed_uint x =
  assert (x = (int_of_packed_varint ** test_packed_gen_parse ** int_to_packed_varint) x)

let test_packed_uint32 x =
  assert (x = (int32_of_packed_varint ** test_packed_gen_parse ** int32_to_packed_varint) x)

let test_packed_uint64 x =
  assert (x = (int64_of_packed_varint ** test_packed_gen_parse ** int64_to_packed_varint) x)


let test_packed_fixed_uint32 x =
  assert (x = (int32_of_packed_fixed32 ** test_packed_gen_parse ** int32_to_packed_fixed32) x)

let test_packed_fixed_uint64 x =
  assert (x = (int64_of_packed_fixed64 ** test_packed_gen_parse ** int64_to_packed_fixed64) x)



let int_input =
  [
    0; 1; 2; 3; -1; -2; -3;
    min_int; min_int + 1; min_int + 2; min_int + 3;
    max_int; max_int - 1; max_int - 2; max_int - 3;
  ]


let uint_input =
  let max_uint = lnot 0 in
  [
    0; 1; 2; 3;
    max_uint;
  ]


open Int32

let int32_input =
  let int_intput = List.map (fun x -> of_int x) int_input in
  int_intput @
  [
    min_int; succ min_int; succ (succ min_int); succ (succ (succ min_int));
    max_int; pred max_int; pred (pred max_int); pred (pred (pred max_int));
  ]


let uint32_input =
  let max_uint = lognot 0l in
  [
    0l; 1l; 2l; 3l;
    max_uint;
  ]


open Int64

let int64_input =
  let int_intput = List.map (fun x -> of_int x) int_input in
  let int32_intput = List.map (fun x -> of_int32 x) int32_input in
  int_intput @
  int32_intput @
  [
    min_int; succ min_int; succ (succ min_int); succ (succ (succ min_int));
    max_int; pred max_int; pred (pred max_int); pred (pred (pred max_int));
  ]


let uint64_input =
  let max_uint = lognot 0L in
  let uint32_intput = List.map (fun x -> int64_of_uint32 x) uint32_input in
  uint32_intput @
  [
    max_uint;
  ]


let max_key = (1 lsl 29) - 1

let key_input = [ 1; 2; 3; max_key - 1; max_key ]


(* TODO:
 * tests for malformed/broken/unexpectedly terminated input
 * tests for OCaml's type overflows
 * tests for cross-type reading, e.g. int64 -> int32, varint -> int64, etc.
 * tests for bools, floats and other types
 *
 *)

let test _ =
  List.iter test_key key_input;

  (* tests for integer fields *)

  List.iter test_int int_input;
  List.iter test_int32 int32_input;
  List.iter test_int64 int64_input;

  List.iter test_fixed_int32 int32_input;
  List.iter test_fixed_int64 int64_input;

  List.iter test_proto_int int_input;
  List.iter test_proto_int32 int32_input;
  List.iter test_proto_int64 int64_input;


  List.iter test_uint uint_input;
  List.iter test_uint32 uint32_input;
  List.iter test_uint64 uint64_input;

  List.iter test_fixed_uint32 uint32_input;
  List.iter test_fixed_uint64 uint64_input;

  (* tests for packed integers *)

  List.iter test_packed_int int_input;
  List.iter test_packed_int32 int32_input;
  List.iter test_packed_int64 int64_input;

  List.iter test_packed_fixed_int32 int32_input;
  List.iter test_packed_fixed_int64 int64_input;

  List.iter test_packed_proto_int int_input;
  List.iter test_packed_proto_int32 int32_input;
  List.iter test_packed_proto_int64 int64_input;


  List.iter test_packed_uint uint_input;
  List.iter test_packed_uint32 uint32_input;
  List.iter test_packed_uint64 uint64_input;

  List.iter test_packed_fixed_uint32 uint32_input;
  List.iter test_packed_fixed_uint64 uint64_input;

  ()


let _ =
  if !Sys.interactive
  then ()
  else test ()

