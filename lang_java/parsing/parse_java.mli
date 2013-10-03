
(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_java.program option * Parser_java.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse :
  Common.filename (*javafile*) -> (program_and_tokens * Parse_info.parsing_stat)

(* may return "just:pb" exn when program2 was a None *)
val parse_program:
  Common.filename -> Ast_java.program

(* for generalized sgrep/spatch patterns *)
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_java.token list

val parse_fuzzy_of:
  string -> Ast_fuzzy.tree list * Parser_java.token list

(* internal *)
val tokens: Common.filename -> Parser_java.token list
(* useful for unit testing, mock *)
val tmp_file_from_string: string -> Common.filename

