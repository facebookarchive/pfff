type t = Json_type.t
open Json_type


(*** Parsing ***)

let check_string_is_utf8 s =
  let encoding =
    if String.length s < 4 then `UTF8
    else Json_lexer.detect_encoding s.[0] s.[1] s.[2] s.[3] in
  if encoding <> `UTF8 then
    json_error "Only UTF-8 encoding is supported" 

let filter_result x =
  Browse.assert_object_or_array x;
  x

let json_of_string 
    ?allow_comments
    ?allow_nan
    ?big_int_mode
    ?(recursive = false)
    s =
  check_string_is_utf8 s;
  let p = Json_lexer.make_param ?allow_comments ?allow_nan ?big_int_mode () in
  let j = 
    Json_parser.main 
      (Json_lexer.token p)
      (Lexing.from_string s)
  in
  if not recursive then filter_result j
  else j


let check_channel_is_utf8 ic =
  let start = pos_in ic in
  let encoding =
    try
      let c1 = input_char ic in
      let c2 = input_char ic in
      let c3 = input_char ic in
      let c4 = input_char ic in
      Json_lexer.detect_encoding c1 c2 c3 c4
    with End_of_file -> `UTF8 in
  if encoding <> `UTF8 then
    json_error "Only UTF-8 encoding is supported";
  (try seek_in ic start
   with _ -> json_error "Not a regular file")

(* from_channel and from_channel4 work 
   only on seekable devices (regular files) *)
let from_channel p recursive file ic = 
  check_channel_is_utf8 ic;
  let lexbuf = Lexing.from_channel ic in
  Json_lexer.set_file_name lexbuf file;
  let j =
    Json_parser.main
      (Json_lexer.token p)
      lexbuf
  in
  if recursive then j
  else filter_result j


let load_json
    ?allow_comments ?allow_nan ?big_int_mode ?(recursive = false)
    file = 
  let ic = open_in file in
  let x =
    let p =
      Json_lexer.make_param ?allow_comments ?allow_nan ?big_int_mode () in
    try `Result (from_channel p recursive file ic)
    with e -> `Exn e in
  close_in ic;
  match x with
      `Result x -> x
    | `Exn e -> raise e


(*** Printing ***)

(* JSON does not allow rendering floats with a trailing dot: that is,
   1234. is not allowed, but 1234.0 is ok.  here, we add a '0' if
   string_of_int result in a trailing dot *)
let fprint_float allow_nan fmt f =
  match classify_float f with
      FP_nan -> 
	if allow_nan then Format.fprintf fmt "NaN"
	else json_error "Not allowed to serialize NaN value"
    | FP_infinite ->
	if allow_nan then
	  if f < 0. then Format.fprintf fmt "-Infinity"
	  else Format.fprintf fmt "Infinity"
	else json_error "Not allowed to serialize infinite value"
    | FP_zero
    | FP_normal
    | FP_subnormal ->
	let s = string_of_float f in
	Format.fprintf fmt "%s" s;
	let s_len = String.length s in
	if s.[ s_len - 1 ] = '.' then
	  Format.fprintf fmt "0"

let escape_json_string buf s =
  for i = 0 to String.length s - 1 do
    let c = String.unsafe_get s i in
    match c with 
      | '"'    -> Buffer.add_string buf "\\\""
      | '\t'   -> Buffer.add_string buf "\\t"
      | '\r'   -> Buffer.add_string buf "\\r"
      | '\b'   -> Buffer.add_string buf "\\b"
      | '\n'   -> Buffer.add_string buf "\\n"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\\'   -> Buffer.add_string buf "\\\\"
   (* | '/'    -> "\\/" *) (* Forward slash can be escaped 
			      but doesn't have to *)
      | '\x00'..'\x1F' (* Control characters that must be escaped *)
      | '\x7F' (* DEL *) -> 
	  Printf.bprintf buf "\\u%04X" (int_of_char c)
      | _      -> 
	  (* Don't bother detecting or escaping multibyte chars *)
	  Buffer.add_char buf c
  done

let fquote_json_string fmt s =
  let buf = Buffer.create (String.length s) in
  escape_json_string buf s;
  Format.fprintf fmt "\"%s\"" (Buffer.contents buf)

let bquote_json_string buf s =
  Printf.bprintf buf "\"%a\"" escape_json_string s

module Compact =
struct
  open Format

  let rec fprint_json allow_nan fmt = function
      Object o -> 
	pp_print_string fmt "{";
	fprint_object allow_nan fmt o;
	pp_print_string fmt "}"
    | Array a -> 
	pp_print_string fmt "[";
	fprint_list allow_nan fmt a;
	pp_print_string fmt "]"
    | Bool b -> 
	pp_print_string fmt (if b then "true" else "false")
    | Null -> 
	pp_print_string fmt "null"
    | Int i -> pp_print_string fmt (string_of_int i)
    | Float f -> pp_print_string fmt (string_of_json_float allow_nan f)
    | String s -> fquote_json_string fmt s
	
  and fprint_list allow_nan fmt = function
      [] -> ()
    | [x] -> fprint_json allow_nan fmt x
    | x :: tl -> 
	fprint_json allow_nan fmt x;
	pp_print_string fmt ","; 
	fprint_list allow_nan fmt tl
	  
  and fprint_object allow_nan fmt = function
      [] -> ()
    | [x] -> fprint_pair allow_nan fmt x
    | x :: tl -> 
	fprint_pair allow_nan fmt x;
	pp_print_string fmt ","; 
	fprint_object allow_nan fmt tl

  and fprint_pair allow_nan fmt (key, x) =
    fquote_json_string fmt key;
    fprintf fmt ":";
    fprint_json allow_nan fmt x

  (* json does not allow rendering floats with a trailing dot: that is,
     1234. is not allowed, but 1234.0 is ok.  here, we add a '0' if
     string_of_int result in a trailing dot *)
  and string_of_json_float allow_nan f =
    let s = string_of_float f in
    let s_len = String.length s in
    if s.[ s_len - 1 ] = '.' then
      s ^ "0"
    else
      s

  let print ?(allow_nan = false) ?(recursive = false) fmt x =
    if not recursive then
      Browse.assert_object_or_array x;
    fprint_json allow_nan fmt x
end


module Fast =
struct
  open Printf
  open Buffer

  (* Contiguous sequence of non-escaped characters are copied to the buffer
     using one call to Buffer.add_substring *)
  let rec buf_add_json_escstr1 buf s k1 l =
    if k1 < l then (
      let k2 = buf_add_json_escstr2 buf s k1 k1 l in
      if k2 > k1 then
	Buffer.add_substring buf s k1 (k2 - k1);
      if k2 < l then (
	let c = String.unsafe_get s k2 in
	( match c with 
	    | '"'    -> Buffer.add_string buf "\\\""
	    | '\t'   -> Buffer.add_string buf "\\t"
	    | '\r'   -> Buffer.add_string buf "\\r"
	    | '\b'   -> Buffer.add_string buf "\\b"
	    | '\n'   -> Buffer.add_string buf "\\n"
	    | '\012' -> Buffer.add_string buf "\\f"
	    | '\\'   -> Buffer.add_string buf "\\\\"
	 (* | '/'    -> "\\/" *) (* Forward slash can be escaped 
				    but doesn't have to *)
	    | '\x00'..'\x1F' (* Control characters that must be escaped *)
	    | '\x7F' (* DEL *) -> 
		        Printf.bprintf buf "\\u%04X" (int_of_char c)
	    | _      -> assert false
	);
	buf_add_json_escstr1 buf s (k2+1) l
      )
    )

  and buf_add_json_escstr2 buf s k1 k2 l =
    if k2 < l then (
      let c = String.unsafe_get s k2 in
      match c with
	| '"' | '\t' | '\r' | '\b' | '\n' | '\012' | '\\' (*| '/'*)
	| '\x00'..'\x1F' | '\x7F' -> k2
	| _ -> buf_add_json_escstr2 buf s k1 (k2+1) l
    )
    else
      l

  and bquote_json_string buf s =
    Buffer.add_char buf '"';
    buf_add_json_escstr1 buf s 0 (String.length s);
    Buffer.add_char buf '"'

  let rec bprint_json allow_nan buf = function
      Object o -> 
	add_string buf "{";
	bprint_object allow_nan buf o;
	add_string buf "}"
    | Array a -> 
	add_string buf "[";
	bprint_list allow_nan buf a;
	add_string buf "]"
    | Bool b -> 
	add_string buf (if b then "true" else "false")
    | Null -> 
	add_string buf "null"
    | Int i -> add_string buf (string_of_int i)
    | Float f -> add_string buf (string_of_json_float allow_nan f)
    | String s -> bquote_json_string buf s
	
  and bprint_list allow_nan buf = function
      [] -> ()
    | [x] -> bprint_json allow_nan buf x
    | x :: tl -> 
	bprint_json allow_nan buf x;
	add_string buf ","; 
	bprint_list allow_nan buf tl
	  
  and bprint_object allow_nan buf = function
      [] -> ()
    | [x] -> bprint_pair allow_nan buf x
    | x :: tl -> 
	bprint_pair allow_nan buf x;
	add_string buf ","; 
	bprint_object allow_nan buf tl

  and bprint_pair allow_nan buf (key, x) =
    bquote_json_string buf key;
    bprintf buf ":";
    bprint_json allow_nan buf x

  (* json does not allow rendering floats with a trailing dot: that is,
     1234. is not allowed, but 1234.0 is ok.  here, we add a '0' if
     string_of_int result in a trailing dot *)
  and string_of_json_float allow_nan f =
    match classify_float f with
	FP_nan -> 
	  if allow_nan then "NaN"
	  else json_error "Not allowed to serialize NaN value"
    | FP_infinite ->
	if allow_nan then
	  if f < 0. then "-Infinity"
	  else "Infinity"
	else json_error "Not allowed to serialize infinite value"
    | FP_zero
    | FP_normal
    | FP_subnormal ->
	let s = string_of_float f in
	let s_len = String.length s in
	if s.[ s_len - 1 ] = '.' then
	  s ^ "0"
	else
	  s

  let print ?(allow_nan = false) ?(recursive = false) buf x =
    if not recursive then
      Browse.assert_object_or_array x;
    bprint_json allow_nan buf x
end



(*** Pretty printing ***)

module Pretty =
struct
  open Format
    
  (* Printing anything but a value in a key:value pair.

     Opening and closing brackets in such arrays and objects
     are aligned vertically if they are not on the same line. 
  *)
  let rec fprint_json allow_nan fmt = function
      Object l -> fprint_object allow_nan fmt l
    | Array l -> fprint_array allow_nan fmt l
    | Bool b -> fprintf fmt "%s" (if b then "true" else "false")
    | Null -> fprintf fmt "null"
    | Int i -> fprintf fmt "%i" i
    | Float f -> fprint_float allow_nan fmt f
    | String s -> fquote_json_string fmt s
	
  (* Printing an array which is not the value in a key:value pair *)
  and fprint_array allow_nan fmt = function
      [] -> fprintf fmt "[]"
    | x :: tl -> 
	fprintf fmt "@[<hv 2>[@ "; 
	fprint_json allow_nan fmt x;
	List.iter (fun x -> 
		     fprintf fmt ",@ ";
		     fprint_json allow_nan fmt x) tl;
	fprintf fmt "@;<1 -2>]@]"
	  
  (* Printing an object which is not the value in a key:value pair *)
  and fprint_object allow_nan fmt = function
      [] -> fprintf fmt "{}"
    | x :: tl -> 
	fprintf fmt "@[<hv 2>{@ "; 
	fprint_pair allow_nan fmt x;
	List.iter (fun x -> 
		     fprintf fmt ",@ ";
		     fprint_pair allow_nan fmt x) tl;
	fprintf fmt "@;<1 -2>}@]"

  (* Printing a key:value pair.

     The opening bracket stays on the same line as the key, no matter what,
     and the closing bracket is either on the same line
     or vertically aligned with the beginning of the key. 
  *)
  and fprint_pair allow_nan fmt (key, x) =
    match x with
	Object l -> 
	  (match l with
	       [] -> fprintf fmt "%a: {}" fquote_json_string key
	     | x :: tl -> 
		 fprintf fmt "@[<hv 2>%a: {@ " fquote_json_string key;
		 fprint_pair allow_nan fmt x;
		 List.iter (fun x -> 
			      fprintf fmt ",@ ";
			      fprint_pair allow_nan fmt x) tl;
		 fprintf fmt "@;<1 -2>}@]")
      | Array l ->
	  (match l with
	       [] -> fprintf fmt "%a: []" fquote_json_string key
	     | x :: tl -> 
		 fprintf fmt "@[<hv 2>%a: [@ " fquote_json_string key;
		 fprint_json allow_nan fmt x;
		 List.iter (fun x -> 
			      fprintf fmt ",@ ";
			      fprint_json allow_nan fmt x) tl;
		 fprintf fmt "@;<1 -2>]@]")
      | _ -> 
	  (* An atom, perhaps a long string that would go to the next line *)
	  fprintf fmt "@[%a:@;<1 2>%a@]" 
	    fquote_json_string key (fprint_json allow_nan) x

  let print ?(allow_nan = false) ?(recursive = false) fmt x =
    if not recursive then
      Browse.assert_object_or_array x;
    fprint_json allow_nan fmt x
end


let string_of_json ?allow_nan ?(compact = false) ?recursive x =
  let buf = Buffer.create 2000 in
  if compact then
    Fast.print ?allow_nan ?recursive buf x
  else
    (let fmt = Format.formatter_of_buffer buf in
     (match recursive with
	  None
	| Some false -> Browse.assert_object_or_array x
	| Some true -> ()
     );
     let allow_nan = match allow_nan with None -> false | Some b -> b in
     Pretty.fprint_json allow_nan fmt x;
     Format.pp_print_flush fmt ());
  Buffer.contents buf

let save_json ?allow_nan ?(compact = false) ?recursive file x =
  let oc = open_out file in
  let print = 
    if compact then Compact.print
    else Pretty.print in
  let fmt = Format.formatter_of_out_channel oc in
  try
    print ?allow_nan ?recursive fmt x;
    Format.pp_print_flush fmt ();
    close_out oc
  with e -> 
    close_out_noerr oc;
    raise e
