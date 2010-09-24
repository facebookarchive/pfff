
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

(* lexer helpers *)
val tokinfo_str_pos: string -> int -> info
val rewrap_str: string -> info -> info

(* reflection *)
val vof_vtoken:
  vtoken -> Ocaml.v

val vtoken_ofv:
  Ocaml.v -> vtoken

