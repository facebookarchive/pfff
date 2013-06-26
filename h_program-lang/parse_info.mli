
(* 'token_location' < 'token_origin' < 'token_mutable' *)

(* regular position information *)
type token_location = {
    str: string; (* the content of the "token" *)
    charpos: int; (* byte position *)
    line: int; column: int;
    file: Common.filename;
} 

(* to deal with expanded tokens, e.g. preprocessor like cpp for C *)
type token_origin =
  | OriginTok  of token_location
  | FakeTokStr of string  * (token_location * int) option (* next to *)
  | ExpandedTok of token_location * token_location * int 
  | Ab (* abstract token, see parse_info.ml comment *)

(* to allow source to source transformation via token "annotations", 
 * see the documentation for spatch.
 *)
type token_mutable = {
  (* contains the position of the token through the token_location embedded
   * inside the token type.
   *)
  mutable token: token_origin; 
  (* for spatch *)
  mutable transfo: transformation;
  (* TODO? *)
  mutable comments: unit; 
}

 and transformation = 
  | NoTransfo
  | Remove 
  | AddBefore of add
  | AddAfter of add
  | Replace of add

  and add = 
    | AddStr of string
    | AddNewlineAndIdent

(* shortcut *)
type info = token_mutable

(* not used but used to be useful in coccinelle *)
type posrv = 
  | Real of token_location 
  | Virt of 
      token_location (* last real info before expanded tok *) * 
      int (* virtual offset *)


(* see also type filepos = { l: int; c: int; } in common.mli *)
val fake_token_location : token_location
val string_of_token_location : token_location -> string
val string_of_token_location_bis : token_location -> string

val info_start_file: Common.filename -> info

type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
}
val default_stat: Common.filename -> parsing_stat
val print_parsing_stat_list: parsing_stat list -> unit


type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
}

val mk_tokens_state: 'tok list -> 'tok tokens_state

(* channel, size, source *)
type changen = unit -> (in_channel * int * Common.filename)

(* Create filename-arged functions from changen-type ones *)
val file_wrap_changen : (changen -> 'a) -> (Common.filename -> 'a)

(* array[i] will contain the (line x col) of the i char position *)
val full_charpos_to_pos : Common.filename -> (int * int) array
val full_charpos_to_pos_from_changen : changen -> (int * int) array
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_token_location : 
  Common.filename -> (int * int) array -> token_location -> token_location
val full_charpos_to_pos_large: 
  Common.filename -> (int -> (int * int))
val full_charpos_to_pos_large_from_changen : changen -> (int -> (int * int))
val complete_token_location_large : 
  Common.filename -> (int -> (int * int))  -> token_location -> token_location

(* return line x col x str_line  from a charpos. This function is quite
 * expensive so don't use it to get the line x col from every token in
 * a file. Instead use full_charpos_to_pos.
 *)
val info_from_charpos : int -> Common.filename -> (int * int * string)

val error_message :       Common.filename -> (string * int) -> string
val error_message_short : Common.filename -> (string * int) -> string
val error_message_token_location :  token_location -> string
val error_message_info :  info -> string
(* add a 'decalage/shift' argument to handle stuff such as cpp which includes 
 * files and who can make shift.
 *)
val error_messagebis : Common.filename -> (string * int) -> int -> string

val print_bad: int -> int * int -> string array -> unit

(* lexer helpers *)
val tokinfo_str_pos: string -> int -> info
val rewrap_str: string -> info -> info
val tok_add_s: string -> info -> info

val token_location_of_info: info -> token_location

val str_of_info: info -> string
val line_of_info: info -> int
val col_of_info: info -> int
val file_of_info: info -> Common.filename
val pos_of_info: info -> int
val pinfo_of_info: info -> token_origin

(* small error reporting, for longer reports use error_message above *)
val string_of_info: info -> string

val is_origintok: info -> bool

(* original info *)
val get_opi: token_origin -> token_location
val get_pi: token_origin -> token_location

(* misc *)
val get_info: (token_location -> 'a) -> info -> 'a
val get_orig_info: (token_location -> 'a) -> info -> 'a

val compare_pos: info -> info -> int
val min_max_ii_by_pos: info list -> info * info

val mk_info_item_DEPRECATED: 
  info_of_tok:('tok -> info) -> 'tok list -> string * 'tok list

val lexbuf_to_strpos:
  Lexing.lexbuf -> string * int

(* meta *)
val vof_token_location: 
  token_location -> Ocaml.v
val vof_token_origin:
  token_origin -> Ocaml.v
val vof_info:
  info -> Ocaml.v

val vof_transformation:
  transformation -> Ocaml.v
