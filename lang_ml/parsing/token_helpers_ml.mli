
val is_eof          : Parser_ml.token -> bool
val is_comment      : Parser_ml.token -> bool

val token_kind_of_tok: Parser_ml.token -> Parse_info.token_kind

val info_of_tok : 
  Parser_ml.token -> Parse_info.info
val visitor_info_of_tok : 
  (Parse_info.info -> Parse_info.info) -> Parser_ml.token -> Parser_ml.token

val line_of_tok  : Parser_ml.token -> int
val str_of_tok   : Parser_ml.token -> string
val file_of_tok  : Parser_ml.token -> Common.filename
val pos_of_tok   : Parser_ml.token -> int

(* for unparsing *)
val elt_of_tok: Parser_ml.token -> Lib_unparser.elt
