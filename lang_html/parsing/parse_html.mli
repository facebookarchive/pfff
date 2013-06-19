
type program2 = Ast_html.html_tree * Parser_html.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse:
  Common.filename -> Ast_html.html_tree * Parser_html.token list


val html_tree_of_string: string -> Ast_html.html_tree

(* using ocamlnet/netstring/nethtml.ml parser *)
(* val parse_simple_tree: Ast_html.html_raw -> Ast_html.html_tree2 *)

