
type program_with_comments = Ast_php.program * Parser_php.token list

exception Parse_error of Parse_info.info

val _hmemo_parse_php: 
  (Common.filename, program_with_comments * Parse_info.parsing_stat) Hashtbl.t

(* This is the main function. raise Parse_error when not Flag.error_recovery.
 *
 * !It can cache the parsing result in _hmemo_parse_php if Flag.caching_parsing
 * is set.
 *)
val parse : 
  ?pp:string option -> 
  Common.filename -> (program_with_comments * Parse_info.parsing_stat)

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

val xdebug_expr_of_string: string -> Ast_php.expr 
val expr_of_string: string -> Ast_php.expr
val program_of_string: string -> Ast_php.program
val tokens_of_string: string -> Parser_php.token list
val any_of_string:  string -> Ast_php.any
val tmp_php_file_from_string: ?header:string -> string -> Common.filename
val tokens: 
  ?init_state:Lexer_php.state_mode ->
  Common.filename -> Parser_php.token list
