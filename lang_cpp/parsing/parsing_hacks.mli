
(* will among other things interally call pp_token.ml to expand some macros *)
val fix_tokens_cpp : 
  macro_defs:(string, Pp_token.define_body) Hashtbl.t ->
  Parser_cpp.token list -> Parser_cpp.token list

(* next stream tokens -> passed stream tokens -> final next token *)
val lookahead : 
  Parser_cpp.token list -> Parser_cpp.token list -> Parser_cpp.token

