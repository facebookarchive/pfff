
type program2 = toplevel2 list
     (* the token list contains also the comment-tokens *)
  and toplevel2 = 
  Ast_ml.toplevel (* NotParsedCorrectly if parse error *) * Parser_ml.token list

exception Parse_error of Parse_info.info

(* This is the main function. See flag_parsing_ml for settings. *)
val parse:
  Common.filename -> (program2 * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_ml.program

(* internal *)
val tokens: Common.filename -> Parser_ml.token list
