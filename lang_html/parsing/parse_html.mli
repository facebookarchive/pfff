
exception Parse_error of Parse_info.info

(* This is the main function *)
val parse:
  Common.filename -> Ast_html.html_tree (* todo  * Parser_html.token list *)

(* using ocamlnet/netstring/nethtml.ml parser *)
val parse_simple_tree: Ast_html.html_raw -> Ast_html.html_tree2
