
type token_extended = {
  mutable t: Parser_cpp.token;
  mutable where : context list;
  mutable new_tokens_before : Parser_cpp.token list;
  line : int;
  col : int;
}
 and context = InFunction | InEnum | InStruct | InInitializer | NoContext

val mk_token_extended : Parser_cpp.token -> token_extended
val rebuild_tokens_extented : token_extended list -> token_extended list

type paren_grouped =
  | Parenthised of paren_grouped list list * token_extended list
  | PToken of token_extended

type brace_grouped =
  | Braceised of brace_grouped list list * token_extended *
      token_extended option
  | BToken of token_extended

type ifdef_grouped =
  | Ifdef of ifdef_grouped list list * token_extended list
  | Ifdefbool of bool * ifdef_grouped list list * token_extended list
  | NotIfdefLine of token_extended list

type 'a line_grouped = Line of 'a list

type body_function_grouped =
  | BodyFunction of token_extended list
  | NotBodyLine of token_extended list

val mk_parenthised: token_extended list -> paren_grouped list
val mk_braceised:   token_extended list -> brace_grouped list
val mk_ifdef:       token_extended list -> ifdef_grouped list
val mk_body_function_grouped: token_extended list -> body_function_grouped list

val mk_line_parenthised: paren_grouped list -> paren_grouped line_grouped list

(* ---------------------------------------------------------------------- *)
val iter_token_paren : (token_extended -> unit) -> paren_grouped list -> unit
val iter_token_brace : (token_extended -> unit) -> brace_grouped list -> unit
val iter_token_ifdef : (token_extended -> unit) -> ifdef_grouped list -> unit

val tokens_of_paren:         paren_grouped list -> token_extended list
val tokens_of_paren_ordered: paren_grouped list -> token_extended list

(* ---------------------------------------------------------------------- *)
val set_context_tag: brace_grouped list -> unit
