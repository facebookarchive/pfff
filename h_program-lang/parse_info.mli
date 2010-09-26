
type vtoken =
  | OriginTok  of Common.parse_info
  | FakeTokStr of string  *
        (Common.parse_info * int) option
  | Ab
  | ExpandedTok of 
      Common.parse_info  *
      Common.parse_info * int 

type info = { 
  (* contains among other things the position of the token through
   * the Common.parse_info embedded inside the pinfo type.
   *)
  mutable pinfo : vtoken; 
  comments: unit; (* TODO *)
  mutable transfo: transformation;
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

type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
}
val default_stat: Common.filename -> parsing_stat

(* lexer helpers *)
val tokinfo_str_pos: string -> int -> info
val rewrap_str: string -> info -> info
val tok_add_s: string -> info -> info

val str_of_info: info -> string
val line_of_info: info -> int
val col_of_info: info -> int
val file_of_info: info -> Common.filename
val pos_of_info: info -> int
val pinfo_of_info: info -> vtoken

val is_origintok: info -> bool

(* reflection *)
val vof_vtoken:
  vtoken -> Ocaml.v
val vtoken_ofv:
  Ocaml.v -> vtoken
