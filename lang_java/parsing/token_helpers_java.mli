val is_eof : Parser_java.token -> bool
val is_comment: Parser_java.token -> bool
val is_just_comment: Parser_java.token -> bool

val info_of_tok : Parser_java.token -> Parse_info.info

val visitor_info_of_tok : 
  (Parse_info.info -> Parse_info.info) -> Parser_java.token -> Parser_java.token

val line_of_tok    : Parser_java.token -> int
val str_of_tok     : Parser_java.token -> string
val file_of_tok    : Parser_java.token -> Common.filename
val pos_of_tok    : Parser_java.token -> int

(* for unparsing *)
val elt_of_tok: Parser_java.token -> Lib_unparser.elt
