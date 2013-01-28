
type program2 = toplevel2 list
     (* the token list contains also the comment-tokens *)
 and toplevel2 = Ast_lisp.toplevel * Parser_lisp.token list

(* This is the main function *)
val parse:
  Common.filename -> (program2 * Parse_info.parsing_stat)


(* internal *)
val tokens: Common.filename -> Parser_lisp.token list

