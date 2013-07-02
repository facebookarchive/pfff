
type program2 = toplevel2 list
  (* the token list contains also the comment-tokens *)
  and toplevel2 = 
    Ast_js.toplevel (* NotParsedCorrectly if parse error *) * 
      Parser_js.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse : 
  Common.filename -> (program2 * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_js.program

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.trees * Parser_js.token list

val program_of_program2: program2 -> Ast_js.program

(* to help write test code *)
val program_of_string: string -> Ast_js.program
val tmp_file_from_string: string -> Common.filename

(* internal *)
val tokens: Common.filename -> Parser_js.token list
