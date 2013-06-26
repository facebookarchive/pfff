
val is_eof          : Parser_erlang.token -> bool
val is_comment      : Parser_erlang.token -> bool

val info_of_tok : 
  Parser_erlang.token -> Ast_erlang.info
val visitor_info_of_tok : 
  (Ast_erlang.info -> Ast_erlang.info) -> Parser_erlang.token -> Parser_erlang.token

val line_of_tok  : Parser_erlang.token -> int
val str_of_tok   : Parser_erlang.token -> string
val file_of_tok  : Parser_erlang.token -> Common.filename
val pos_of_tok   : Parser_erlang.token -> int
