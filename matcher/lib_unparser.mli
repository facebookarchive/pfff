
type elt =
  | OrigElt of string
  | Removed of string
  | Added of string
  | Esthet of esthet
  and esthet =
   | Comment of string
   | Newline
   | Space of string

(* helpers *)
val elts_of_any:
  elt_and_info_of_tok:
  ('tok -> elt * Parse_info.info) -> elt list -> 'tok list -> elt list

(* debugging *)
val vof_elt: elt -> Ocaml.v

(* heuristics *)
val drop_esthet_between_removed: elt list -> elt list
val drop_whole_line_if_only_removed: elt list -> elt list

val debug: bool ref

(* main entry point *)
val string_of_toks_using_transfo:
  elt_and_info_of_tok:('tok -> elt * Parse_info.info) -> 'tok list -> string
