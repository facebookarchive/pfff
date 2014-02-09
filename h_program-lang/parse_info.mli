
(* ('token_location' < 'token_origin' < 'token_mutable') * token_kind *)

(* to report errors, regular position information *)
type token_location = {
    str: string; (* the content of the "token" *)
    charpos: int; (* byte position *)
    line: int; column: int;
    file: Common.filename;
} 
(* see also type filepos = { l: int; c: int; } in common.mli *)

(* to deal with expanded tokens, e.g. preprocessor like cpp for C *)
type token_origin =
  | OriginTok  of token_location
  | FakeTokStr of string  * (token_location * int) option (* next to *)
  | ExpandedTok of token_location * token_location * int 
  | Ab (* abstract token, see parse_info.ml comment *)

(* to allow source to source transformation via token "annotations", 
 * see the documentation for spatch
 *)
type token_mutable = {
  token: token_origin; 
  (* for spatch *)
  mutable transfo: transformation;
}

 and transformation = 
  | NoTransfo
  | Remove 
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

  and add = 
    | AddStr of string
    | AddNewlineAndIdent

(* shortcut *)
type info = token_mutable

(* mostly for the fuzzy AST builder *)
type token_kind =
  | LPar | RPar
  | LBrace | RBrace
  | Esthet of esthet
  | Eof
  | Other
  and esthet =
   | Comment
   | Newline
   | Space


val fake_token_location : token_location

val str_of_info   : info -> string
val line_of_info  : info -> int
val col_of_info   : info -> int
val pos_of_info   : info -> int
val file_of_info  : info -> Common.filename

(* small error reporting, for longer reports use error_message above *)
val string_of_info: info -> string
(* meta *)
val vof_info: info -> Ocaml.v

val is_origintok: info -> bool

val token_location_of_info: info -> token_location
val get_original_token_location: token_origin -> token_location

val compare_pos: info -> info -> int
val min_max_ii_by_pos: info list -> info * info

type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
  (* used only for cpp for now *)
  mutable have_timeout: bool;
  mutable commentized: int;
  mutable problematic_lines: (string list * int ) list;
}
val default_stat: Common.filename -> parsing_stat
val print_parsing_stat_list: ?verbose:bool -> parsing_stat list -> unit
val print_recurring_problematic_tokens: parsing_stat list -> unit


(* lexer helpers *)
type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
}
val mk_tokens_state: 'tok list -> 'tok tokens_state

val tokinfo_str_pos: 
  string -> int -> info
val lexbuf_to_strpos:
  Lexing.lexbuf -> string * int
val rewrap_str: string -> info -> info
val tok_add_s: string -> info -> info

(* f(i) will contain the (line x col) of the i char position *)
val full_charpos_to_pos_large: 
  Common.filename -> (int -> (int * int))
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_token_location_large : 
  Common.filename -> (int -> (int * int))  -> token_location -> token_location

val error_message : Common.filename -> (string * int) -> string
val error_message_info :  info -> string
val print_bad: int -> int * int -> string array -> unit

(* channel, size, source *)
type changen = unit -> (in_channel * int * Common.filename)
(* Create filename-arged functions from changen-type ones *)
val file_wrap_changen : (changen -> 'a) -> (Common.filename -> 'a)
val full_charpos_to_pos_large_from_changen : changen -> (int -> (int * int))
