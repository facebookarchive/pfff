
val is_eof          : Parser_python.token -> bool
val is_comment      : Parser_python.token -> bool

val info_of_tok : 
  Parser_python.token -> Parse_info.info
val visitor_info_of_tok : 
  (Parse_info.info -> Parse_info.info) -> Parser_python.token -> Parser_python.token
