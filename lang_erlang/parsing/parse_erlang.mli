
type program2 = toplevel2 list
     (* the token list contains also the comment-tokens *)
  and toplevel2 = 
    Ast_erlang.toplevel (* NotParsedCorrectly if parse error *) * 
      Parser_erlang.token list

(* This is the main function *)
val parse:
  Common.filename -> (program2 * Parse_info.parsing_stat)

(* internal *)
val tokens: Common.filename -> Parser_erlang.token list
