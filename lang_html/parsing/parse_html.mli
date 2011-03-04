
(* This is the main function *)
val parse:
  Common.filename -> Ast_html.html_tree * Parser_html.token list

(* using ocamlnet/netstring/nethtml.ml parser *)
val parse_simple_tree: Ast_html.html_raw -> Ast_html.html_tree2

(* internal *)
val tokens: Common.filename -> Parser_html.token list
