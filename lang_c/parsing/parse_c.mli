
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_c.program option * Parser_cpp.token list

val parse: 
  Common.filename -> (program_and_tokens * Statistics_parsing.parsing_stat)

val parse_program:
  Common.filename -> Ast_c.program
