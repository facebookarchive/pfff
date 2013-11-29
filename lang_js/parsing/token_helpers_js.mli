
val is_eof          : Parser_js.token -> bool
val is_comment      : Parser_js.token -> bool
val is_just_comment : Parser_js.token -> bool

val token_kind_of_tok: Parser_js.token -> Parse_info.token_kind

val info_of_tok : 
  Parser_js.token -> Parse_info.info
val visitor_info_of_tok : 
  (Parse_info.info -> Parse_info.info) -> Parser_js.token -> Parser_js.token

val line_of_tok  : Parser_js.token -> int
