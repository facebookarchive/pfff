
val parse: Common.filename -> Ast_clang.program

(* internal *)
val tokens: Common.filename -> Parser_clang.token list
val tokens_of_string: string -> Parser_clang.token list

val str_of_enum: Ast_clang.enum -> string
