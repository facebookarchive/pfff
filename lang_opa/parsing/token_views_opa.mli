
type token = Parser_opa.token

type tree =
  | T of token
  | Paren of tree list list (* grouped by comma *)
  | Brace of tree list list (* grouped by comma too, as in type defs *)
  | Bracket of tree list list
  | Xml of tree list (* attributes *) * tree list (* children *)

val mk_tree: 
  Parser_opa.token list -> tree list

val vof_tree_list: 
  tree list -> Ocaml.v
