(*s: parse_php.mli *)

(*s: type parsing_stat *)
(*e: type parsing_stat *)
(*s: type program2 *)
type program2 = toplevel2 list
  and toplevel2 = 
    Ast_php.toplevel (* NotParsedCorrectly if parse error *) * info_item
     (* the token list contains also the comment-tokens *)
     and info_item = (string * Parser_php.token list)
(*e: type program2 *)

(* This is the main function *)
val parse : 
  ?pp:string option -> 
  Common.filename -> (program2 * Parse_info.parsing_stat)

val parse_program:
  ?pp:string option ->
  Common.filename -> Ast_php.program

(*s: extra parse function signature *)
val xdebug_expr_of_string: string -> Ast_php.expr 
val class_def_of_string: string -> Ast_php.class_def
(*x: extra parse function signature *)
val expr_of_string: string -> Ast_php.expr
val xhp_expr_of_string: string -> Ast_php.expr
val program_of_string: string -> Ast_php.program
(*e: extra parse function signature *)
(*x: parse_php.mli *)
val program_of_program2 : program2 -> Ast_php.program
(*x: parse_php.mli *)
val tokens: Common.filename -> Parser_php.token list
(*x: parse_php.mli *)
(*e: parse_php.mli *)
