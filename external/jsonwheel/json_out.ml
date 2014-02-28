type t = Json_type.t
open Json_type

(* pad: copy paste of printing and pretty printing section of json_io.ml *)

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


let string_of_json ?allow_nan (*?(compact = false) ?recursive *) x =
  let buf = Buffer.create 2000 in
 (*
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
 *)
  let fmt = Format.formatter_of_buffer buf in
  let allow_nan = match allow_nan with None -> false | Some b -> b in
  Pretty.fprint_json allow_nan fmt x;
  Format.pp_print_flush fmt ();

  Buffer.contents buf
 
