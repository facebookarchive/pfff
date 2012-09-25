
(* the token list contains also the comment-tokens *)
type program2 = Ast_java.program option * Parser_java.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse : 
  Common.filename (*javafile*) -> (program2 * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_java.program

(* internal *)
val tokens: Common.filename -> Parser_java.token list
