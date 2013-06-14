(*s: parse_php.mli *)

(*s: type program2 *)
type program2 = toplevel2 list
  (* the token list contains also the comment-tokens *)
  and toplevel2 = Ast_php.toplevel * Parser_php.token list
type program_with_comments = program2
(*e: type program2 *)

exception Parse_error of Parse_info.info

val _hmemo_parse_php: 
  (Common.filename, program2 * Parse_info.parsing_stat) Hashtbl.t

(* This is the main function. raise Parse_error when not Flag.error_recovery.
 *
 * !It can cache the parsing result in _hmemo_parse_php if Flag.caching_parsing
 * is set.
 *)
val parse : 
  ?pp:string option -> 
  Common.filename -> (program2 * Parse_info.parsing_stat)

val parse_program:
  ?pp:string option ->
  Common.filename -> Ast_php.program

val ast_and_tokens: 
  Common.filename -> Ast_php.program * Parser_php.token list

(* for sgrep/spatch patterns *)
val parse_any:
  Common.filename -> Ast_php.any

(* for generalized sgrep/spatch patterns *)
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_php.token list

val parse_fast:
  Common.filename -> Ast_php.program

(*s: extra parse function signature *)
val xdebug_expr_of_string: string -> Ast_php.expr 
(*x: extra parse function signature *)
val expr_of_string: string -> Ast_php.expr
val program_of_string: string -> Ast_php.program
val tokens_of_string: string -> Parser_php.token list
val any_of_string:  string -> Ast_php.any
val tmp_php_file_from_string: string -> Common.filename
(*e: extra parse function signature *)
(*x: parse_php.mli *)
val program_of_program2 : program2 -> Ast_php.program
val program_of_program_with_comments : program_with_comments -> Ast_php.program
(*x: parse_php.mli *)
val tokens: 
  ?init_state:Lexer_php.state_mode ->
  Common.filename -> Parser_php.token list
(*e: parse_php.mli *)
