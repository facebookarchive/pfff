
type parse_info = {
    str: string;
    charpos: int;

    line: int;
    column: int;
    file: Common.filename;
  } 

type info = {
  (* contains among other things the position of the token through
   * the Common.parse_info embedded inside the pinfo type.
   *)
  mutable token: token; 
  mutable comments: unit; (* TODO *)
  mutable transfo: transformation;
}

 and token =
  | OriginTok  of parse_info
  | FakeTokStr of string  * (parse_info * int) option
  | ExpandedTok of parse_info *  parse_info * int 
  | Ab

 and transformation = 
  | NoTransfo
  | Remove 
  | AddBefore of add
  | AddAfter of add
  | Replace of add

  and add = 
    | AddStr of string
    | AddNewlineAndIdent

type posrv = 
  | Real of parse_info 
  | Virt of 
      parse_info (* last real info before expanded tok *) * 
      int (* virtual offset *)



(* see also type filepos = { l: int; c: int; } in common.mli *)
val fake_parse_info : parse_info
val string_of_parse_info : parse_info -> string
val string_of_parse_info_bis : parse_info -> string

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


(* array[i] will contain the (line x col) of the i char position *)
val full_charpos_to_pos : Common.filename -> (int * int) array
(* fill in the line and column field of parse_info that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_parse_info : 
  Common.filename -> (int * int) array -> parse_info -> parse_info
val full_charpos_to_pos_large: 
  Common.filename -> (int -> (int * int))
val complete_parse_info_large : 
  Common.filename -> (int -> (int * int))  -> parse_info -> parse_info

(* return line x col x str_line  from a charpos. This function is quite
 * expensive so don't use it to get the line x col from every token in
 * a file. Instead use full_charpos_to_pos.
 *)
val info_from_charpos : int -> Common.filename -> (int * int * string)

val error_message :       Common.filename -> (string * int) -> string
val error_message_short : Common.filename -> (string * int) -> string
val error_message_parse_info :  parse_info -> string
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

val parse_info_of_info: info -> parse_info

val str_of_info: info -> string
val line_of_info: info -> int
val col_of_info: info -> int
val file_of_info: info -> Common.filename
val pos_of_info: info -> int
val pinfo_of_info: info -> token

val is_origintok: info -> bool

(* original info *)
val get_opi: token -> parse_info
val get_pi: token -> parse_info

(* misc *)
val get_info: (parse_info -> 'a) -> info -> 'a
val get_orig_info: (parse_info -> 'a) -> info -> 'a

val compare_pos: info -> info -> int
val min_max_ii_by_pos: info list -> info * info

val mk_info_item: 
  info_of_tok:('tok -> info) -> 'tok list -> string * 'tok list

val lexbuf_to_strpos:
  Lexing.lexbuf -> string * int

(* meta *)
val vof_token:
  token -> Ocaml.v
val vof_info:
  info -> Ocaml.v
val vof_parse_info: 
  parse_info -> Ocaml.v
val vof_transformation:
  transformation -> Ocaml.v

val token_ofv:
  Ocaml.v -> token

val v_pinfo: 
  token -> unit
val v_transformation: 
  transformation -> unit
