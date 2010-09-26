
val is_eof          : Parser_nw.token -> bool
val is_comment      : Parser_nw.token -> bool

val info_of_tok : 
  Parser_nw.token -> Ast_nw.info
val visitor_info_of_tok : 
  (Ast_nw.info -> Ast_nw.info) -> Parser_nw.token -> Parser_nw.token

val line_of_tok  : Parser_nw.token -> int
val str_of_tok   : Parser_nw.token -> string
val file_of_tok  : Parser_nw.token -> Common.filename

(*val pos_of_tok   : Parser_nw.token -> int*)
