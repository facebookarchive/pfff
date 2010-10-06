
val is_eof          : Parser_ml.token -> bool
val is_comment      : Parser_ml.token -> bool

val info_of_tok : 
  Parser_ml.token -> Ast_ml.info
val visitor_info_of_tok : 
  (Ast_ml.info -> Ast_ml.info) -> Parser_ml.token -> Parser_ml.token

val line_of_tok  : Parser_ml.token -> int
val str_of_tok   : Parser_ml.token -> string
val file_of_tok  : Parser_ml.token -> Common.filename
val pos_of_tok   : Parser_ml.token -> int

(*val pos_of_tok   : Parser_ml.token -> int*)
val pinfo_of_tok   : Parser_ml.token -> Parse_info.token
