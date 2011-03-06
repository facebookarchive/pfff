
type program2 = Ast_css.stylesheet * Parser_css.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse:
  Common.filename -> Ast_css.stylesheet * Parser_css.token list

(* internal *)
val tokens: Common.filename -> Parser_css.token list
