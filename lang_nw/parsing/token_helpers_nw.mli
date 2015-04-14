
val is_eof          : Lexer_nw.token -> bool
val is_comment      : Lexer_nw.token -> bool

val info_of_tok : 
  Lexer_nw.token -> Ast_nw.info
val visitor_info_of_tok : 
  (Ast_nw.info -> Ast_nw.info) -> Lexer_nw.token -> Lexer_nw.token

val line_of_tok  : Lexer_nw.token -> int
val str_of_tok   : Lexer_nw.token -> string
val file_of_tok  : Lexer_nw.token -> Common.filename

(*val pos_of_tok   : Lexer_nw.token -> int*)
