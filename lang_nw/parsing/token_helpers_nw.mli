
val is_eof          : Lexer_nw.token -> bool
val is_comment      : Lexer_nw.token -> bool

val token_kind_of_tok: Lexer_nw.token -> Parse_info.token_kind

val info_of_tok : 
  Lexer_nw.token -> Parse_info.info
val visitor_info_of_tok : 
  (Parse_info.info -> Parse_info.info) -> Lexer_nw.token -> Lexer_nw.token
