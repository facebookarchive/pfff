
val is_eof          : Parser_python.token -> bool
val is_comment      : Parser_python.token -> bool

val info_of_tok : 
  Parser_python.token -> Ast_python.info
val visitor_info_of_tok : 
  (Ast_python.info -> Ast_python.info) -> Parser_python.token -> Parser_python.token

val line_of_tok  : Parser_python.token -> int
val str_of_tok   : Parser_python.token -> string
val file_of_tok  : Parser_python.token -> Common.filename
val pos_of_tok   : Parser_python.token -> int

