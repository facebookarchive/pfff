
type program_and_tokens = Ast_opa.program * Parser_opa.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse: Common.filename -> program_and_tokens

(* degenerated parser *)
val parse_just_tokens: Common.filename -> program_and_tokens

(* internal *)
val tokens: Common.filename -> Parser_opa.token list
