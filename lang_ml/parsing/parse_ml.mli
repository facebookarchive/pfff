
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_ml.program option * Parser_ml.token list

exception Parse_error of Parse_info.info

(* This is the main function. See flag_parsing_ml for settings. *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_ml.program

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_ml.token list

(* internal *)
val tokens: Common.filename -> Parser_ml.token list
