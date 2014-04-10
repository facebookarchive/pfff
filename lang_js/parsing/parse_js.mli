
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_js.program option * Parser_js.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse: 
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_js.program

val parse_string : 
  string -> Ast_js.program

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.trees * Parser_js.token list

(* to help write test code *)
val program_of_string: string -> Ast_js.program

(* internal *)
val tokens: Common.filename -> Parser_js.token list
