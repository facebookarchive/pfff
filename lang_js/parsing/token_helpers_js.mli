
val is_eof          : Parser_js.token -> bool
val is_comment      : Parser_js.token -> bool
val is_just_comment : Parser_js.token -> bool

val token_kind_of_tok: Parser_js.token -> Parse_info.token_kind

val info_of_tok : 
  Parser_js.token -> Ast_js.tok
val visitor_info_of_tok : 
  (Ast_js.tok -> Ast_js.tok) -> Parser_js.token -> Parser_js.token

val str_of_tok   : Parser_js.token -> string
val line_of_tok  : Parser_js.token -> int
val pos_of_tok   : Parser_js.token -> int
val file_of_tok  : Parser_js.token -> Common.filename

(* for unparsing *)
val elt_of_tok: Parser_js.token -> Lib_unparser.elt
