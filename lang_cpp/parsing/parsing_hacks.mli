(* This module tries to detect some cpp idioms so that we can parse as-is
 * files by adjusting or commenting some tokens.
 * Sometime we use some indentation information,  sometimes we
 * do some kind of lalr(k) by finding patterns. We often try to
 * work on a better token representation, like ifdef-paren-ized, brace-ized,
 * paren-ized, so that we can pattern match more easily
 * complex cpp idiom pattern (cf token_views_cpp.ml).
 * We also try to get more contextual information such as whether the
 * token is in an initializer because many patterns are different
 * depending on context.
 * 
 * Example of cpp idioms:
 *  - if 0 for commenting stuff (not always code, sometimes just real comments)
 *  - ifdef old version
 *  - ifdef funheader
 *  - ifdef statements, ifdef expression, ifdef-mid
 *  - macro toplevel (with or without a trailing ';')
 *  - macro foreach
 *  - macro higher order
 *  - macro declare
 *  - macro debug
 *  - macro no ';'
 *  - macro string, and macro function string taking param and ##
 *  - macro attribute
 * 
 * Cf the TMacroXxx in parser_c.mly and MacroXxx in ast_c.ml
 * 
 * We also try to infer typedef.
 * c++ext: we also try to infer templates, constructors, etc.
 * 
 * We also do other stuff involving cpp like expanding macros,
 * and we try parse define body by finding the end of define virtual 
 * end-of-line token. But now most of the code is actually in pp_token.ml
 * It is related to what is in the yacfe configuration file (e.g. standard.h)
 *)

val regexp_macro: Str.regexp
val regexp_annot: Str.regexp
val regexp_declare: Str.regexp
val regexp_foreach: Str.regexp
val regexp_typedef: Str.regexp

(* can reset this global *)
(* todo: val ifdef_paren_cnt: int ref *)

(* will among other things interally call Pp_token to macro
 * expand some macros *)
val fix_tokens_cpp : 
  macro_defs:(string, Pp_token.define_body) Hashtbl.t ->
  Parser_cpp.token list -> Parser_cpp.token list

(* next stream tokens -> passed stream tokens -> final next token *)
val lookahead : 
  Parser_cpp.token list -> Parser_cpp.token list -> Parser_cpp.token

(* ------------------------------------------------------------------------ *)
(* Parsing hack helpers related to #define or #include *)
(* ------------------------------------------------------------------------ *)

val fix_tokens_define : 
  Parser_cpp.token list -> Parser_cpp.token list

(* called when need to pass some tokens during some error recovery *)
val drop_until_defeol: Parser_cpp.token list -> Parser_cpp.token list
val comment_until_defeol: Parser_cpp.token list -> Parser_cpp.token list

(* generates TIncludeStart and TIncludeFilename tokens *)
val tokens_include: 
  Ast_cpp.info * string * string * bool ref ->
  Parser_cpp.token * Parser_cpp.token list

