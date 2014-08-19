
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_lisp.program option * Parser_lisp.token list

exception Parse_error of string * Parse_info.info

(* This is the main function *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_lisp.program


(* internal *)
val tokens: Common.filename -> Parser_lisp.token list

