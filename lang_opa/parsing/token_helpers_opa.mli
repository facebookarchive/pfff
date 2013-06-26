
val is_eof          : Parser_opa.token -> bool
val is_comment      : Parser_opa.token -> bool

val info_of_tok : 
  Parser_opa.token -> Ast_opa.tok
val visitor_info_of_tok : 
  (Ast_opa.tok -> Ast_opa.tok) -> Parser_opa.token -> Parser_opa.token

val line_of_tok  : Parser_opa.token -> int
val str_of_tok   : Parser_opa.token -> string
val file_of_tok  : Parser_opa.token -> Common.filename
val pos_of_tok   : Parser_opa.token -> int
