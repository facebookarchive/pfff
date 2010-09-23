
val is_eof          : Parser_js.token -> bool
val is_comment      : Parser_js.token -> bool
val is_just_comment : Parser_js.token -> bool

val info_of_tok : 
  Parser_js.token -> Ast_js.info
val visitor_info_of_tok : 
  (Ast_js.info -> Ast_js.info) -> Parser_js.token -> Parser_js.token

val line_of_tok  : Parser_js.token -> int
val str_of_tok   : Parser_js.token -> string
val file_of_tok  : Parser_js.token -> Common.filename
val pos_of_tok   : Parser_js.token -> int

val pinfo_of_tok   : Parser_js.token -> Ast_js.pinfo
val is_origin : Parser_js.token -> bool
