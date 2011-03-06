
val info_of_tok : 
  Parser_css.token -> Ast_css.info
val visitor_info_of_tok : 
  (Ast_css.info -> Ast_css.info) -> Parser_css.token -> Parser_css.token

val line_of_tok  : Parser_css.token -> int
val str_of_tok   : Parser_css.token -> string
val file_of_tok  : Parser_css.token -> Common.filename
val pos_of_tok   : Parser_css.token -> int
