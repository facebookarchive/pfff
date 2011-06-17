
(* transform TInclude, filter the TCppEscapedNewline, generate TIdentDefine
 * and other related fresh tokens.
 *)
val fix_tokens_define : 
  Parser_cpp.token list -> Parser_cpp.token list

(* called when need to pass some tokens during some error recovery *)
val drop_until_defeol: Parser_cpp.token list -> Parser_cpp.token list
val comment_until_defeol: Parser_cpp.token list -> Parser_cpp.token list

(* generates TIncludeStart and TIncludeFilename tokens *)
val tokens_include: 
  Ast_cpp.info * string * string * bool ref ->
  Parser_cpp.token * Parser_cpp.token list

