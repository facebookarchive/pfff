
type vtoken =
  | OriginTok  of Common.parse_info
  | FakeTokStr of string  *
        (Common.parse_info * int) option
  | Ab
  | ExpandedTok of 
      Common.parse_info  *
      Common.parse_info * int 


val vof_vtoken:
  vtoken -> Ocaml.v

val vtoken_ofv:
  Ocaml.v -> vtoken

