
type program2 = toplevel2 list
  and toplevel2 = 
    Ast_js.toplevel (* NotParsedCorrectly if parse error *) * info_item
     (* the token list contains also the comment-tokens *)
     and info_item = (string * Parser_js.token list)

(* This is the main function *)
val parse : 
  Common.filename -> (program2 * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_js.program

val program_of_program2: program2 -> Ast_js.program

(* internal *)
val tokens: Common.filename -> Parser_js.token list
