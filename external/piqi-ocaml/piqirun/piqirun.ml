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

(* Runtime support for piqi/Protocol Buffers wire format encoding
 *
 * Encoding rules follow this specification:
 *
 *   http://code.google.com/apis/protocolbuffers/docs/encoding.html
 *)


(*
 * Runtime support for parsers (decoders).
 *
 *)

exception Error of int * string


let string_of_loc pos =
  string_of_int pos


let strerr loc s = 
  string_of_loc loc ^ ": " ^ s


let buf_error loc s =
  (*
  failwith (strerr s loc)
  *)
  raise (Error (loc, s))


let error obj s =
  let loc = -1 in (* TODO, XXX: obj location db? *)
  buf_error loc s


type string_slice =
  {
    s : string;
    start_pos : int;  (* position of `s` in the input stream *)
    len :int;
    mutable pos : int;
  }


module IBuf =
  struct
    type t =
      | String of string_slice
      | Channel of in_channel


    let of_channel x = Channel x


    let of_string x start_pos =
      String
        { s = x; len = String.length x;
          start_pos = start_pos; pos = 0; 
        }


    let to_string buf =
      match buf with
        | String x ->
            (* XXX, TODO: try to avoid extra alloaction if the buffer holds the
             * whole desired string? *)
            String.sub x.s x.pos (x.len - x.pos)
        | Channel x ->
            (* XXX: optimize using block reads? OTOH, it seems like this
             * function is not supposed to be called for channels at all *)
            let res = Buffer.create 20 in
            try
              while true (* this cycle exist only on End_of_file exception *)
              do
                Buffer.add_char res (input_char x)
              done; ""
            with End_of_file ->
              Buffer.contents res


    let pos buf =
      match buf with
        | String x -> x.pos + x.start_pos
        | Channel x -> pos_in x


    let size buf =
      match buf with
        | String x -> x.len - x.pos
        | Channel x ->
            (* this function should is not called for channels *)
            assert false


    let error buf s =
      let loc = pos buf in
      buf_error loc s


    exception End_of_buffer


    (* get the next byte from the buffer and return it as an integer *)
    let next_byte buf =
      match buf with
        | String x ->
            if x.pos >= x.len
            then
              raise End_of_buffer
            else
              let res = x.s.[x.pos] in
              x.pos <- x.pos + 1;
              Char.code res
        | Channel x ->
            (try input_byte x
             with End_of_file -> raise End_of_buffer)


    (* get the next [length] bytes the buffer and return it as a string *)
    let next_block buf length =
      match buf with
        | String x ->
            if x.pos + length > x.len
            then
              (* XXX: adjusting position to provide proper EOB location *)
              (x.pos <- x.len; raise End_of_buffer)
            else
              (* NOTE: start_pos, pos and the string itself remain the same in
               * the new buffer *)
              let res = String { x with len = x.pos + length } in
              (* skip the new buffer in the current buffer *)
              x.pos <- x.pos + length;
              res
        | Channel x ->
            let start_pos = pos_in x in
            let s = String.create length in
            (try Pervasives.really_input x s 0 length
             with End_of_file -> raise End_of_buffer
            );
            of_string s start_pos


    let of_string x =
      of_string x 0
  end


type t = 
  | Varint of int
  | Varint64 of int64 (* used if int width is not enough *)
  | Int32 of int32
  | Int64 of int64
  | Block of IBuf.t
  | Top_block of IBuf.t (* top-level block *)


(* initializers for embedded records/variants (i.e. their contents start without
 * any leading headers/delimiters/separators) *)
let init_from_channel ch =
  Top_block (IBuf.of_channel ch)


let init_from_string s =
  Top_block (IBuf.of_string s)


let error_variant obj code =
  error obj ("unknown variant: " ^ string_of_int code)
let error_missing obj code =
  error obj  ("missing field: " ^ string_of_int code)

let error_enum_const obj = error obj "unknown enum constant"


(* TODO, XXX: issue warning on unparsed fields or change behaviour depending on
 * "strict" config option ? *)
let check_unparsed_fields l =
  ()
  (*
  List.iter (fun (code, x) -> error code "unknown field") l
  *)


let next_varint_byte buf =
  let x = IBuf.next_byte buf in
  (* msb indicating that more bytes will follow *)
  let msb = x land 0x80 in
  let x = x land 0x7f in
  msb, x


let parse_varint64 i buf msb x partial_res =
  let rec aux i msb x res =
    let x = Int64.of_int x in
    let y = Int64.shift_left x (i*7) in
    if (Int64.shift_right_logical y (i*7)) <> x
    then
      IBuf.error buf "integer overflow while reading varint"
    else
      let res = Int64.logor res y in
      if msb = 0
      then Varint64 res (* no more octets => return *)
      else
        let msb, x = next_varint_byte buf in
        aux (i+1) msb x res (* continue reading octets *)
  in aux i msb x (Int64.of_int partial_res)


(* TODO: optimize using Sys.word_size and manual cycle unrolling *)
let parse_varint_common buf i res =
  let rec aux i res =
    let msb, x = next_varint_byte buf in
    let y = x lsl (i*7) in
    (* NOTE: by using asr rather than lsr we disallow signed integers to appear
     * in Varints, they will rather be returned as Varint64 *)
    if y asr (i*7) <> x
    then
      (* switch to Varint64 in case of overflow *)
      parse_varint64 i buf msb x res
    else
      let res = res lor y in
      if msb = 0
      then Varint res (* no more octets => return *)
      else aux (i+1) res (* continue reading octets *)
  in
  try aux i res
  with IBuf.End_of_buffer ->
    IBuf.error buf "unexpected end of buffer while reading varint"


let parse_varint buf =
  parse_varint_common buf 0 0


let try_parse_varint buf =
  (* try to read the first byte and don't handle End_of_buffer exception *)
  let msb, x = next_varint_byte buf in
  if msb = 0
  then Varint x (* no more octets => return *)
  else parse_varint_common buf 1 x


(* TODO, XXX: check signed overflow *)
(* TODO: optimize for little-endian achitecture *)
let parse_fixed32 buf =
  try
    let res = ref 0l in
    for i = 0 to 3
    do
      let x = IBuf.next_byte buf in
      let x = Int32.of_int x in
      let x = Int32.shift_left x (i*8) in
      res := Int32.logor !res x
    done;
    !res
  with IBuf.End_of_buffer ->
    IBuf.error buf "unexpected end of buffer while reading fixed32"


let parse_fixed64 buf =
  try
    let res = ref 0L in
    for i = 0 to 7
    do
      let x = IBuf.next_byte buf in
      let x = Int64.of_int x in
      let x = Int64.shift_left x (i*8) in
      res := Int64.logor !res x
    done;
    !res
  with IBuf.End_of_buffer ->
    IBuf.error buf "unexpected end of buffer while reading fixed64"


let try_parse_fixed32 buf =
  (* try to read the first byte and don't handle End_of_buffer exception *)
  let b1 = IBuf.next_byte buf in
  let res = ref (Int32.of_int b1) in
  try
    for i = 1 to 3
    do
      let x = IBuf.next_byte buf in
      let x = Int32.of_int x in
      let x = Int32.shift_left x (i*8) in
      res := Int32.logor !res x
    done;
    !res
  with IBuf.End_of_buffer ->
    IBuf.error buf "unexpected end of buffer while reading fixed32"


let try_parse_fixed64 buf =
  (* try to read the first byte and don't handle End_of_buffer exception *)
  let b1 = IBuf.next_byte buf in
  let res = ref (Int64.of_int b1) in
  try
    for i = 1 to 7
    do
      let x = IBuf.next_byte buf in
      let x = Int64.of_int x in
      let x = Int64.shift_left x (i*8) in
      res := Int64.logor !res x
    done;
    !res
  with IBuf.End_of_buffer ->
    IBuf.error buf "unexpected end of buffer while reading fixed64"


let parse_block buf =
  (* XXX: is there a length limit or it is implementation specific? *)
  match parse_varint buf with
    | Varint length when length >= 0 ->
        (try IBuf.next_block buf length
         with IBuf.End_of_buffer -> error buf "unexpected end of block")
    | Varint _ | Varint64 _ ->
        IBuf.error buf "block length is too long"
    | _ -> assert false


(* TODO: optimize using Sys.word_size *)
let parse_field_header buf =
  (* the range for field codes is 1 - (2^29 - 1) which mean on 32-bit
   * machine ocaml's int may not hold the full value *)
  match try_parse_varint buf with
    | Varint key ->
        let wire_type = key land 7 in
        let field_code = key lsr 3 in
        wire_type, field_code

    | Varint64 key when Int64.logand key 0xffff_ffff_0000_0000L <> 0L ->
        IBuf.error buf "field code is too big"

    | Varint64 key ->
        let wire_type = Int64.to_int (Int64.logand key 7L) in
        let field_code = Int64.to_int (Int64.shift_right_logical key 3) in
        wire_type, field_code
    | _ -> assert false


let parse_field buf =
  try
    let wire_type, field_code = parse_field_header buf in
    let field_value =
      match wire_type with
        | 0 -> parse_varint buf
        | 1 -> Int64 (parse_fixed64 buf)
        | 2 -> Block (parse_block buf)
        | 5 -> Int32 (parse_fixed32 buf)
        | 3 | 4 -> IBuf.error buf "groups are not supported"
        | _ -> IBuf.error buf ("unknown wire type " ^ string_of_int wire_type)
    in
    Some (field_code, field_value)
  with
    IBuf.End_of_buffer -> None


(* parse header of a top-level value of a primitive type (i.e. generated with a
 * special "-1" code) *)
let parse_toplevel_header buf =
  match parse_field buf with
    | None ->
        error buf "unexpected end of buffer when reading top-level header"
    | Some (field_code, field_value) ->
        if field_code = 1
        then field_value
        else error buf "invalid top-level header for a primitive type"


let rec expect_int32 = function
  | Int32 i -> i
  | Top_block buf -> expect_int32 (parse_toplevel_header buf)
  | obj -> error obj "fixed32 expected"


let rec expect_int64 = function
  | Int64 i -> i
  | Top_block buf -> expect_int64 (parse_toplevel_header buf)
  | obj -> error obj "fixed64 expected"


(*
 * Convert Zig-zag varint to normal varint
 *)

let rec zigzag_varint_of_varint = function
  | Varint x ->
      let sign = - (x land 1) in
      let res = (x lsr 1) lxor sign in
      Varint res
  | Varint64 x ->
      let sign = Int64.neg (Int64.logand x 1L) in
      let res = Int64.logxor (Int64.shift_right_logical x 1) sign in
      Varint64 res
  | Top_block buf -> zigzag_varint_of_varint (parse_toplevel_header buf)
  | obj -> error obj "varint expected"


(*
 * Parsing primitive types
 *)


let max_uint =
  match Sys.word_size with
    | 32 -> 0x0000_0000_7fff_ffffL (* on 32-bit, int is 31-bit wide *)
    | 64 -> 0x7fff_ffff_ffff_ffffL (* on 64-bit, int is 63-bit wide *)
    | _ -> assert false


let int64_of_uint x =
  (* prevent turning into a negative value *)
  Int64.logand (Int64.of_int x) max_uint

let int64_of_uint32 x =
  (* prevent turning into a negative value *)
  Int64.logand (Int64.of_int32 x) 0x0000_0000_ffff_ffffL


(* this encoding is only for unsigned integers *)
let rec int_of_varint obj =
  match obj with
    | Varint x -> x
    | Varint64 x ->
        let res = Int64.to_int x in
        if int64_of_uint res <> x
        then error obj "int overflow in 'int_of_varint'";
        res
    | Top_block buf -> int_of_varint (parse_toplevel_header buf)
    | _ ->
        error obj "varint expected"


let rec int_of_signed_varint obj =
  match obj with
    | Varint x -> x
    | Varint64 x ->
        let res = Int64.to_int x in
        if Int64.of_int res <> x
        then error obj "int overflow in 'int_of_signed_varint'";
        res
    | Top_block buf -> int_of_signed_varint (parse_toplevel_header buf)
    | _ ->
        error obj "varint expected"


(* this encoding is only for signed integers *)
let int_of_zigzag_varint x =
  int_of_signed_varint (zigzag_varint_of_varint x)

let int_of_fixed32 x =
  Int32.to_int (expect_int32 x)

let int_of_fixed64 x =
  Int64.to_int (expect_int64 x)


(* this encoding is only for unsigned integers *)
let rec int64_of_varint = function
  | Varint x -> int64_of_uint x
  | Varint64 x -> x
  | Top_block buf -> int64_of_varint (parse_toplevel_header buf)
  | obj -> error obj "varint expected"


let rec int64_of_signed_varint = function
  | Varint x -> Int64.of_int x
  | Varint64 x -> x
  | Top_block buf -> int64_of_signed_varint (parse_toplevel_header buf)
  | obj -> error obj "varint expected"


(* this encoding is only for signed integers *)
let int64_of_zigzag_varint x =
  int64_of_signed_varint (zigzag_varint_of_varint x)


let int64_of_fixed32 x =
  let x = expect_int32 x in
  int64_of_uint32 x

let int64_of_fixed64 = expect_int64

let int64_of_signed_fixed32 x = Int64.of_int32 (expect_int32 x)

let int64_of_signed_fixed64 = int64_of_fixed64


(* this encoding is only for unsigned integers *)
let rec int32_of_varint obj =
  match obj with
    | Varint x ->
        (* don't bother handling separate cases for now: which type is wider --
         * int32 or int *)
        int32_of_varint (Varint64 (int64_of_uint x))
    | Varint64 x ->
        let res = Int64.to_int32 x in
        if int64_of_uint32 res <> x
        then error obj "int32 overflow in 'int32_of_varint'";
        res
    | Top_block buf -> int32_of_varint (parse_toplevel_header buf)
    | obj ->
        error obj "varint expected"


let rec int32_of_signed_varint obj =
  match obj with
    | Varint x ->
        (* don't bother handling separate cases for now: which type is wider --
         * int32 or int *)
        int32_of_signed_varint (Varint64 (Int64.of_int x))
    | Varint64 x ->
        let res = Int64.to_int32 x in
        if Int64.of_int32 res <> x
        then error obj "int32 overflow in 'int32_of_signed_varint'";
        res
    | Top_block buf -> int32_of_signed_varint (parse_toplevel_header buf)
    | obj ->
        error obj "varint expected"


(* this encoding is only for signed integers *)
let int32_of_zigzag_varint x =
  int32_of_signed_varint (zigzag_varint_of_varint x)

let int32_of_fixed32 = expect_int32

let int32_of_signed_fixed32 = int32_of_fixed32


let float_of_int32 x =
  Int32.float_of_bits x (* XXX *)

let float_of_int64 x =
  Int64.float_of_bits x (* XXX *)

let float_of_fixed64 buf = 
  float_of_int64 (expect_int64 buf)

let float_of_fixed32 buf = 
  float_of_int32 (expect_int32 buf)


let bool_of_varint obj =
  match int_of_varint obj with
    | 0 -> false
    | 1 -> true
    | _ -> error obj "invalid boolean constant"

let parse_bool_field = bool_of_varint


let rec parse_binary_field obj =
  match obj with
    | Block buf -> IBuf.to_string buf
    | Top_block buf -> parse_binary_field (parse_toplevel_header buf)
    | obj -> error obj "block expected"

let validate_string s = s (* XXX: validate utf8-encoded string *)

let parse_string_field obj =
  validate_string (parse_binary_field obj)

let string_of_block = parse_string_field
let word_of_block = parse_string_field (* word is encoded as string *)
let text_of_block = parse_string_field (* text is encoded as string *)


(*
 * Parsing packed fields (packed encoding is used only for primitive
 * numeric types)
 *)


let int_of_packed_varint buf =
  int_of_varint (try_parse_varint buf)

let int_of_packed_signed_varint buf =
  int_of_signed_varint (try_parse_varint buf)

let int_of_packed_zigzag_varint buf =
  int_of_zigzag_varint (try_parse_varint buf)

let int_of_packed_fixed32 buf =
  Int32.to_int (try_parse_fixed32 buf)

let int_of_packed_fixed64 buf =
  Int64.to_int (try_parse_fixed64 buf)


let int64_of_packed_varint buf =
  int64_of_varint (try_parse_varint buf)

let int64_of_packed_signed_varint buf =
  int64_of_signed_varint (try_parse_varint buf)

let int64_of_packed_zigzag_varint buf =
  int64_of_zigzag_varint (try_parse_varint buf)

let int64_of_packed_fixed64 buf =
  try_parse_fixed64 buf

let int64_of_packed_fixed32 buf =
  let x = try_parse_fixed32 buf in
  int64_of_uint32 x

let int64_of_packed_signed_fixed64 = int64_of_packed_fixed64

let int64_of_packed_signed_fixed32 buf =
  Int64.of_int32 (try_parse_fixed32 buf)


let int32_of_packed_varint buf =
  int32_of_varint (try_parse_varint buf)

let int32_of_packed_signed_varint buf =
  int32_of_signed_varint (try_parse_varint buf)

let int32_of_packed_zigzag_varint buf =
  int32_of_zigzag_varint (try_parse_varint buf)

let int32_of_packed_fixed32 buf =
  try_parse_fixed32 buf

let int32_of_packed_signed_fixed32 = int32_of_packed_fixed32


let float_of_packed_fixed32 buf =
  float_of_int32 (try_parse_fixed32 buf)

let float_of_packed_fixed64 buf =
  float_of_int64 (try_parse_fixed64 buf)


let bool_of_packed_varint buf =
  bool_of_varint (try_parse_varint buf)


(*
 * Parsing complex user-defined types
 *)

let parse_record_buf buf =
  let rec parse_unordered accu =
    match parse_field buf with
      | Some field ->
          parse_unordered (field::accu)
      | None ->
          let res = List.rev accu in
          (* stable-sort the obtained fields by codes: it is safe to use
           * subtraction, because field codes are 29-bit integers *)
          List.stable_sort (fun (a, _) (b, _) -> a - b) res
  in
  let rec parse_ordered accu =
    match parse_field buf with
      | Some ((code, _value) as field) ->
          (* check if the fields appear in order *)
          (match accu with
            | (prev_code, _)::_ when prev_code > code ->
                (* the field is out of order *)
                parse_unordered (field::accu)
            | _ ->
                parse_ordered (field::accu)
          )
      | None ->
          List.rev accu
  in
  parse_ordered []


let parse_record obj =
  match obj with
    | Block buf
    | Top_block buf -> parse_record_buf buf
    | obj -> error obj "block expected"


let parse_variant obj = 
  match parse_record obj with
    | [x] -> x
    | [] -> error obj "empty variant"
    | _ -> error obj "variant contains more than one option"


(* find all fields with the given code in the list of fields sorted by codes *)
let find_fields code l =
  let rec aux accu = function
    | (code', obj)::t when code' = code ->
        aux (obj::accu) t
    | (code', obj)::t when code' < code ->
        (* skipping the field which code is less than the requested one *)
        aux accu t
    | rem ->
        List.rev accu, rem
  in
  aux [] l


(* find the last instance of a field given its code in the list of fields sorted
 * by codes *)
let find_field code l =
  let rec try_find_next_field prev_value = function
    | (code', value)::t when code' = code -> (* field is found again *)
        try_find_next_field value t
    | rem -> (* previous field was the last one *)
        Some prev_value, rem
  in
  let rec find_first_field = function
    | (code', value)::t when code' = code -> (* field is found *)
        (* check if this is the last instance of it, if not, continue iterating
         * through the list *)
        try_find_next_field value t
    | (code', _)::t when code' < code ->
        (* skipping the field which code is less than the requested one *)
        find_first_field t
    | rem -> (* not found *)
        None, rem

  in
  find_first_field l


let parse_binobj parse_fun binobj =
  let buf = init_from_string binobj in
  parse_fun buf


let parse_default binobj =
  let buf = init_from_string binobj in
  buf


(* XXX, NOTE: using default with requried or optional-default fields *)
let parse_required_field code parse_value ?default l =
  let res, rem = find_field code l in
  match res with
    | None ->
        (match default with
           | Some x -> parse_value (parse_default x), l
           | None -> error_missing l code)
    | Some x ->
        parse_value x, rem


let parse_optional_field code parse_value ?default l =
  let res, rem = find_field code l in
  match res with
    | None ->
        (match default with
           | Some x -> Some (parse_value (parse_default x)), l
           | None -> None, l)
    | Some x ->
        Some (parse_value x), rem


let parse_repeated_field code parse_value l =
  let res, rem = find_fields code l in
  List.map parse_value res, rem


(* similar to List.map but store results in a newly created output array *)
let map_l2a f l =
  let len = List.length l in
  (* create and initialize the results array *)
  let a = Array.make len (Obj.magic 1) in
  let rec aux i = function
    | [] -> ()
    | h::t ->
        a.(i) <- f h;
        aux (i+1) t
  in
  aux 0 l; a


let parse_repeated_array_field code parse_value l =
  let res, rem = find_fields code l in
  map_l2a parse_value res, rem


let parse_packed_fields parse_packed_value buf =
  let rec aux accu =
    try
      (* try parsing another packed element *)
      let value = parse_packed_value buf in
      aux (value :: accu)
    with IBuf.End_of_buffer -> (* no more packed elements *)
      (* NOTE: accu is returned in reversed order and will reversed to a normal
       * order at a later stage in rev_flatmap *)
      accu
  in
  aux []


let parse_packed_field parse_packed_value parse_value obj =
  match obj with
    | Block buf ->
        parse_packed_fields parse_packed_value buf
    | _ ->
        [parse_value obj]


let parse_packed_array_field elem_size parse_packed_value buf =
  let size = IBuf.size buf in
  let elem_count = size / elem_size in

  (* make sure the array contains whole elements w/o any trailing fractions *)
  if size mod elem_size <> 0
  then IBuf.error buf "invalid packed fixed-width field";

  (* create a new array for results *)
  let a = Array.make elem_count (Obj.magic 1) in
  (* parse packed elements and store resuts in the array *)
  for i = 0 to elem_count - 1
  do
    a.(i) <- parse_packed_value buf
  done;
  (* return the resulting array *)
  a


(* the same as List.flatten (List.map (fun x -> List.rev (f x)) l), but more
 * efficient and tail recursive *)
let rev_flatmap f l =
  let l = List.rev_map f l in
  List.fold_left (fun accu x -> List.rev_append x accu) [] l


let parse_packed_repeated_field code parse_packed_value parse_value l =
  let fields, rem = find_fields code l in
  let res = rev_flatmap (parse_packed_field parse_packed_value parse_value) fields in
  res, rem


let parse_packed_repeated_array_field code parse_packed_value parse_value l =
  let res, rem = parse_packed_repeated_field code parse_packed_value parse_value l in
  Array.of_list res, rem


let parse_packed_repeated_array_fixed_field elem_size code parse_packed_value parse_value l =
  let fields, rem = find_fields code l in
  match fields with
    | [Block buf] ->
        let res = parse_packed_array_field elem_size parse_packed_value buf in
        res, rem
    | _ ->
        (* this is the case when there are several repeated entries with the
         * same code each containing packed repeated values -- need to handle
         * this case, but not optimizing for it *)
        parse_packed_repeated_array_field code parse_packed_value parse_value l


let parse_packed_repeated_array32_field code parse_packed_value parse_value l =
  parse_packed_repeated_array_fixed_field 4 code parse_packed_value parse_value l

let parse_packed_repeated_array64_field code parse_packed_value parse_value l =
  parse_packed_repeated_array_fixed_field 8 code parse_packed_value parse_value l


let parse_flag code l =
  let res, rem = find_field code l in
  match res with
    | None -> false, l
    | Some x ->
        (match parse_bool_field x with
          | true -> true, rem
          | false -> error x "invalid encoding for a flag")


let parse_list_elem parse_value (code, x) =
  (* NOTE: expecting "1" as list element code *)
  if code = 1
  then parse_value x
  else error x "invalid list element code"


let parse_list parse_value obj =
  let l = parse_record obj in
  List.map (parse_list_elem parse_value) l


let parse_array parse_value obj =
  let l = parse_record obj in
  map_l2a (parse_list_elem parse_value) l


let parse_packed_list_1 parse_packed_value parse_value fields =
  rev_flatmap (parse_list_elem (parse_packed_field parse_packed_value parse_value)) fields


let parse_packed_list parse_packed_value parse_value obj =
  let fields = parse_record obj in
  parse_packed_list_1 parse_packed_value parse_value fields


let parse_packed_array parse_packed_value parse_value obj =
  let res = parse_packed_list parse_packed_value parse_value obj in
  Array.of_list res


let parse_packed_array_fixed elem_size parse_packed_value parse_value obj =
  let l = parse_record obj in
  match l with
    | [1, Block buf] ->
        parse_packed_array_field elem_size parse_packed_value buf
    | _ ->
        (* this is the case when there are several list entries each containing
         * packed repeated values -- need to handle this case, but not
         * optimizing for it *)
        let res = parse_packed_list_1 parse_packed_value parse_value l in
        Array.of_list res


let parse_packed_array32 parse_packed_value parse_value obj =
  parse_packed_array_fixed 4 parse_packed_value parse_value obj

let parse_packed_array64 parse_packed_value parse_value obj =
  parse_packed_array_fixed 8 parse_packed_value parse_value obj


(*
 * Runtime support for generators (encoders)
 *)

module OBuf =
  struct
    (* auxiliary iolist type and related primitives *)
    type t =
        Ios of string
      | Iol of t list
      | Iol_size of int * (t list) (* iolist with known size *)
      | Iob of char
      | IBuf of IBuf.t


    let ios x = Ios x
    let iol l = Iol l
    let iob b = Iob b


    (* iolist buf output *)
    let to_buffer0 buf l =
      let rec aux = function
        | Ios s -> Buffer.add_string buf s
        | Iol l | Iol_size (_, l) -> List.iter aux l
        | Iob b -> Buffer.add_char buf b
        | IBuf (IBuf.String x) -> Buffer.add_substring buf x.s x.pos (x.len - x.pos)
        | IBuf (IBuf.Channel x) -> assert false
      in aux l


    (* iolist output size *)
    let rec size = function
      | Ios s -> String.length s
      | Iol l -> List.fold_left (fun accu x -> accu + (size x)) 0 l
      | Iol_size (size, _) -> size
      | Iob _ -> 1
      | IBuf x -> IBuf.size x


    let iol_size l =
      let n = size (Iol l) in
      Iol_size (n, l)


    let iol_known_size n l =
      Iol_size (n, l)


    let to_string l =
      let buf = Buffer.create (size l) in
      to_buffer0 buf l;
      Buffer.contents buf


    let to_buffer l =
      let buf = Buffer.create 80 in
      to_buffer0 buf l;
      buf


    let to_channel ch code =
      let buf = to_buffer code in
      Buffer.output_buffer ch buf
  end


open OBuf


let to_string = OBuf.to_string
let to_buffer = OBuf.to_buffer
let to_channel = OBuf.to_channel


let iob i = (* IO char represented as Ios '_' *)
  iob (Char.chr i)


(*
 * Generating varint values and fields
 *)

let gen_varint64_value x =
  let rec aux x =
    let b = Int64.to_int (Int64.logand x 0x7FL) in (* base 128 *)
    let rem = Int64.shift_right_logical x 7 in
    (* Printf.printf "x: %LX, byte: %X, rem: %LX\n" x b rem; *)
    if rem = 0L
    then [iob b]
    else
      begin
        (* set msb indicating that more bytes will follow *)
        let b = b lor 0x80 in
        (iob b) :: (aux rem)
      end
  in iol (aux x)


let gen_unsigned_varint_value x =
  let rec aux x =
    let b = x land 0x7F in (* base 128 *)
    let rem = x lsr 7 in
    if rem = 0
    then [iob b]
    else
      begin
        (* set msb indicating that more bytes will follow *)
        let b = b lor 0x80 in
        (iob b) :: (aux rem)
      end
  in iol (aux x)


let gen_signed_varint_value x =
  (* negative varints are encoded as bit-complement 64-bit varints, always
   * producing 10-bytes long value *)
  if x < 0
  then gen_varint64_value (Int64.of_int x)
  else gen_unsigned_varint_value x


let gen_unsigned_varint32_value x =
  let rec aux x =
    let b = Int32.to_int (Int32.logand x 0x7Fl) in (* base 128 *)
    let rem = Int32.shift_right_logical x 7 in
    if rem = 0l
    then [iob b]
    else
      begin
        (* set msb indicating that more bytes will follow *)
        let b = b lor 0x80 in
        (iob b) :: (aux rem)
      end
  in iol (aux x)


let gen_signed_varint32_value x =
  (* negative varints are encoded as bit-complement 64-bit varints, always
   * producing 10-bytes long value *)
  if Int32.compare x 0l < 0 (* x < 0? *)
  then gen_varint64_value (Int64.of_int32 x)
  else gen_unsigned_varint32_value x


let gen_key ktype code =
  (* make sure that the field code is in the valid range *)
  assert (code < 1 lsl 29 && code >= 1);
  if code land (1 lsl 28) <> 0 && Sys.word_size == 32
  then
    (* prevent an overflow of 31-bit OCaml integer on 32-bit platform *)
    let ktype = Int32.of_int ktype in
    let code = Int32.of_int code in
    let x = Int32.logor ktype (Int32.shift_left code 3) in
    gen_unsigned_varint32_value x
  else
    gen_unsigned_varint_value (ktype lor (code lsl 3))


(* gen key for primitive types *)
let gen_primitive_key ktype code =
  (* -1 is a special code meaning that values of primitive types must be
   * generated with a field header with code 1: (abs (-1)) == 1
   *
   * This way, "-1" is treated the same as "1", leading to a uniform interface
   * with generators for length-delimited types.
   *
   * For types which values are encoded as length-delimited blocks (i.e.
   * records, variants, lists), -1 means suppress generation of a surrounding
   * field header that includes the key and the length of data (see generators
   * for these types below) *)
  gen_key ktype (abs code)


let gen_signed_varint_field code x =
  iol [
    gen_primitive_key 0 code;
    gen_signed_varint_value x;
  ]

let gen_varint_field code x =
  iol [
    gen_primitive_key 0 code;
    gen_unsigned_varint_value x;
  ]

let gen_signed_varint32_field code x =
  iol [
    gen_primitive_key 0 code;
    gen_signed_varint32_value x;
  ]

let gen_varint32_field code x =
  iol [
    gen_primitive_key 0 code;
    gen_unsigned_varint32_value x;
  ]

let gen_varint64_field code x =
  iol [
    gen_primitive_key 0 code;
    gen_varint64_value x;
  ]


(*
 * Generating fixed32 and fixed64 values and fields
 *)

let gen_fixed32_value x = (* little-endian *)
  let s = String.create 4 in
  let x = ref x in
  for i = 0 to 3
  do
    let b = Char.chr (Int32.to_int (Int32.logand !x 0xFFl)) in
    s.[i] <- b;
    x := Int32.shift_right_logical !x 8
  done;
  ios s


let gen_fixed64_value x = (* little-endian *)
  let s = String.create 8 in
  let x = ref x in
  for i = 0 to 7
  do
    let b = Char.chr (Int64.to_int (Int64.logand !x 0xFFL)) in
    s.[i] <- b;
    x := Int64.shift_right_logical !x 8
  done;
  ios s


let gen_fixed32_field code x =
  iol [
    gen_primitive_key 5 code;
    gen_fixed32_value x;
  ]


let gen_fixed64_field code x =
  iol [
    gen_primitive_key 1 code;
    gen_fixed64_value x;
  ]


(*
 * Zig-zag encoding for int, int32 and int64
 *)


let zigzag_of_int x =
  (* encode signed integer using ZigZag encoding;
   * NOTE: using arithmetic right shift *)
  (x lsl 1) lxor (x asr 62) (* XXX: can use lesser value than 62 on 32 bit? *)


let zigzag_of_int32 x =
  (* encode signed integer using ZigZag encoding;
   * NOTE: using arithmetic right shift *)
  Int32.logxor (Int32.shift_left x 1) (Int32.shift_right x 31)


let zigzag_of_int64 x =
  (* encode signed integer using ZigZag encoding;
   * NOTE: using arithmetic right shift *)
  Int64.logxor (Int64.shift_left x 1) (Int64.shift_right x 63)


(*
 * Public Piqi runtime functions for generating primitive types
 *)


let int_to_varint code x =
  gen_varint_field code x

let int_to_signed_varint code x =
  gen_signed_varint_field code x

let int_to_zigzag_varint code x =
  gen_varint_field code (zigzag_of_int x)


let int64_to_varint code x =
  gen_varint64_field code x

let int64_to_signed_varint = int64_to_varint

let int64_to_zigzag_varint code x =
  int64_to_varint code (zigzag_of_int64 x)

let int64_to_fixed64 code x =
  gen_fixed64_field code x

let int64_to_fixed32 code x =
  gen_fixed32_field code (Int64.to_int32 x)

let int64_to_signed_fixed64 = int64_to_fixed64

let int64_to_signed_fixed32 = int64_to_fixed32


let int32_to_varint code x =
  gen_varint32_field code x

let int32_to_signed_varint code x =
  gen_signed_varint32_field code x

let int32_to_zigzag_varint code x =
  gen_varint32_field code (zigzag_of_int32 x)

let int32_to_fixed32 code x =
  gen_fixed32_field code x

let int32_to_signed_fixed32 = int32_to_fixed32


let int32_of_float x =
  Int32.bits_of_float x (* XXX *)

let int64_of_float x =
  Int64.bits_of_float x (* XXX *)


let float_to_fixed32 code x =
  gen_fixed32_field code (int32_of_float x)

let float_to_fixed64 code x =
  gen_fixed64_field code (int64_of_float x)


let int_of_bool = function
  | true -> 1
  | false -> 0

let bool_to_varint code x =
  gen_varint_field code (int_of_bool x)

let gen_bool_field = bool_to_varint


let gen_string_field code s =
  let contents = ios s in
  iol [
    gen_primitive_key 2 code;
    gen_unsigned_varint_value (String.length s);
    contents;
  ]

let string_to_block = gen_string_field
let binary_to_block = gen_string_field (* binaries use the same encoding as strings *)
let word_to_block = gen_string_field (* word is encoded as string *)
let text_to_block = gen_string_field (* text is encoded as string *)


(* the inverse of parse_field *)
let gen_parsed_field (code, value) =
  match value with
    | Varint x ->
        gen_varint_field code x
    | Varint64 x ->
        gen_varint64_field code x
    | Int32 x ->
        gen_fixed32_field code x
    | Int64 x ->
        gen_fixed64_field code x
    | Block x ->
        iol [
          gen_primitive_key 2 code;
          gen_unsigned_varint_value (IBuf.size x);
          IBuf x
        ]
    | Top_block x ->  (* impossible clause *)
        assert false


let gen_parsed_field_list l =
  List.map gen_parsed_field l


(*
 * Generating packed fields (packed encoding is used only for primitive
 * numeric types)
 *)


let int_to_packed_varint x =
  gen_unsigned_varint_value x

let int_to_packed_signed_varint x =
  gen_signed_varint_value x

let int_to_packed_zigzag_varint x =
  gen_unsigned_varint_value (zigzag_of_int x)


let int64_to_packed_varint x =
  gen_varint64_value x

let int64_to_packed_signed_varint x =
  gen_varint64_value x

let int64_to_packed_zigzag_varint x =
  gen_varint64_value (zigzag_of_int64 x)

let int64_to_packed_fixed64 x =
  gen_fixed64_value x

let int64_to_packed_fixed32 x =
  gen_fixed32_value (Int64.to_int32 x)

let int64_to_packed_signed_fixed64 = int64_to_packed_fixed64

let int64_to_packed_signed_fixed32 = int64_to_packed_fixed32


let int32_to_packed_varint x =
  gen_unsigned_varint32_value x

let int32_to_packed_signed_varint x =
  gen_signed_varint32_value x

let int32_to_packed_zigzag_varint x =
  gen_unsigned_varint32_value (zigzag_of_int32 x)

let int32_to_packed_fixed32 x =
  gen_fixed32_value x

let int32_to_packed_signed_fixed32 = int32_to_packed_fixed32


let float_to_packed_fixed32 x =
  gen_fixed32_value (int32_of_float x)

let float_to_packed_fixed64 x =
  gen_fixed64_value (int64_of_float x)


let bool_to_packed_varint x =
  gen_unsigned_varint_value (int_of_bool x)


(*
 * Generating complex user-defined types
 *)

let gen_required_field code f x = f code x


let gen_optional_field code f = function
  | Some x -> f code x
  | None -> iol []


let gen_repeated_field code f l =
  iol (List.map (f code) l)


(* similar to Array.map but produces list instead of array *)
let map_a2l f a =
  let rec aux i accu =
    if i < 0
    then accu
    else
      let res = f a.(i) in
      aux (i-1) (res::accu)
  in
  aux ((Array.length a) - 1) []


let gen_repeated_array_field code f l =
  iol (map_a2l (f code) l)


let gen_packed_repeated_field_common code contents =
  let size = OBuf.size contents in
  if size = 0
  then contents (* don't generate anything for empty repeated packed field *)
  else
    iol [
      gen_key 2 code;
      gen_unsigned_varint_value size;
      contents;
    ]


let gen_packed_repeated_field code f l =
  let contents = iol_size (List.map f l) in
  gen_packed_repeated_field_common code contents


let gen_packed_repeated_array_field code f l =
  let contents = iol_size (map_a2l f l) in
  gen_packed_repeated_field_common code contents


let gen_packed_repeated_array32_field code f l =
  let size = 4 * Array.length l in
  let contents = iol_known_size size (map_a2l f l) in
  gen_packed_repeated_field_common code contents


let gen_packed_repeated_array64_field code f l =
  let size = 8 * Array.length l in
  let contents = iol_known_size size (map_a2l f l) in
  gen_packed_repeated_field_common code contents


let gen_flag code x =
  match x with
    | false -> iol [] (* no flag *)
    | true -> gen_bool_field code true


let gen_record code contents =
  let contents = iol_size contents in
  (* special code meaning that key and length sould not be generated *)
  if code = -1
  then contents
  else
    iol [
      gen_key 2 code;
      (* the length of fields data *)
      gen_unsigned_varint_value (OBuf.size contents);
      contents;
    ]


(* generate binary representation of <type>_list .proto structure *)
let gen_list f code l =
  (* NOTE: using "1" as list element code *)
  let contents = List.map (f 1) l in
  gen_record code contents


let gen_array f code l =
  (* NOTE: using "1" as list element code *)
  let contents = map_a2l (f 1) l in
  gen_record code contents


let gen_packed_list f code l =
  (* NOTE: using "1" as list element code *)
  let field = gen_packed_repeated_field 1 f l in
  gen_record code [field]


let gen_packed_array f code l =
  let field = gen_packed_repeated_array_field 1 f l in
  gen_record code [field]

let gen_packed_array32 f code l =
  let field = gen_packed_repeated_array32_field 1 f l in
  gen_record code [field]

let gen_packed_array64 f code l =
  let field = gen_packed_repeated_array64_field 1 f l in
  gen_record code [field]


let gen_binobj gen_obj x =
  let obuf = gen_obj (-1) x in
  (* return the result encoded as a binary string *)
  OBuf.to_string obuf


(* generate length-delimited block of data. The inverse operation to
 * parse_block() below *)
let gen_block iodata =
  iol [
      gen_unsigned_varint_value (OBuf.size iodata);
      iodata;
  ]


(* XXX, TODO: return Some or None on End_of_buffer *)
let parse_block buf =
  Top_block (parse_block buf)

