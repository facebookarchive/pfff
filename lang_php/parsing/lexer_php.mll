(*s: lexer_php.mll *)
{
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: Facebook copyright *)

open Common 

(*s: basic pfff module open and aliases *)
open Ast_php 

module Ast = Ast_php
module Flag = Flag_parsing_php
(*e: basic pfff module open and aliases *)

open Parser_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The PHP lexer.
 *
 * There are a few tricks to go around ocamllex restrictions
 * because PHP has different lexing rules depending on some "context"
 * (similar to Perl, e.g. the <<<END context).
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_lexing 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical of string

(*s: lexer helpers *)
(* pad: hack around ocamllex to emulate the yyless of flex. It seems
 * to work.
 *)
let yyless n lexbuf = 
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { currp with
    Lexing.pos_cnum = currp.Lexing.pos_cnum - n;
  }
(*x: lexer helpers *)
let tok lexbuf = 
  Lexing.lexeme lexbuf

let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

(*x: lexer helpers *)
let tok_add_s s ii  =
  Ast.rewrap_str ((Ast.str_of_info ii) ^ s) ii
(*e: lexer helpers *)


let xhp_or_t_ident ii fii = 
  if !Flag.xhp_builtin 
  then fii ii 
  else 
    let s = Ast.str_of_info ii in
    T_IDENT(s, ii)

let lang_ext_or_t_ident ii fii =
  if !Flag.facebook_lang_extensions
  then fii ii
  else 
    let s = Ast.str_of_info ii in
    T_IDENT(s, ii)

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
(*s: keywords_table hash *)
(* opti: less convenient, but using a hash is faster than using a match.
 * Note that PHP allow those keywords to be used in certain places,
 * for instance as object flds as in $o->while, so the transformation
 * from a LABEL to those keywords is done only in a few cases.
 * 
 * note: PHP is case insensitive so this hash table is used on
 * a lowercased string so don't put strings in uppercase below because
 * such key would never be reached!
 *)
let keyword_table = Common.hash_of_list [

  "while",   (fun ii -> T_WHILE ii);   "endwhile", (fun ii -> T_ENDWHILE ii);
  "do",      (fun ii -> T_DO ii);
  "for",     (fun ii -> T_FOR ii);     "endfor", (fun ii -> T_ENDFOR ii);
  "foreach", (fun ii -> T_FOREACH ii); "endforeach",(fun ii -> T_ENDFOREACH ii);

  "class_xdebug",    (fun ii -> T_CLASS_XDEBUG ii);
  "resource_xdebug", (fun ii -> T_RESOURCE_XDEBUG ii);

  (* Those tokens were not in the original PHP lexer. This allowed to 
   * have "self"/"parent" to be used at more places, e.g. as a function 
   * name which is tolerated by PHP but should not IMHO. Those idents
   * have a special meaning and this should be reflected in the lexer,
   * especially since PHP 5.3 which allows static:: in addition to 
   * self::, parent::. 'static' is a keyword so there is no reason 
   * to not make self/parent keywords too.
   * 
   * todo: should do something similar for $this.
   *)
  "self", (fun ii -> T_SELF ii); "parent", (fun ii -> T_PARENT ii);

 (*s: repetitive keywords table *)
  "if",       (fun ii -> T_IF ii);     "else", (fun ii -> T_ELSE ii);
  "elseif",   (fun ii -> T_ELSEIF ii); "endif",      (fun ii -> T_ENDIF ii);
  "break",    (fun ii -> T_BREAK ii);  "continue",   (fun ii -> T_CONTINUE ii);
  "switch",   (fun ii -> T_SWITCH ii); "endswitch",(fun ii -> T_ENDSWITCH ii);
  "case",       (fun ii -> T_CASE ii); "default",    (fun ii -> T_DEFAULT ii);

  "return",     (fun ii -> T_RETURN ii);

  "try",        (fun ii -> T_TRY ii); "catch",      (fun ii -> T_CATCH ii);
  "throw",      (fun ii -> T_THROW ii);

  "exit",       (fun ii -> T_EXIT ii); "die",        (fun ii -> T_EXIT ii);

  "array",      (fun ii -> T_ARRAY ii); "list",       (fun ii -> T_LIST ii);
  "as",         (fun ii -> T_AS ii);

  "include",(fun ii ->T_INCLUDE ii);"include_once",(fun ii ->T_INCLUDE_ONCE ii);
  "require",(fun ii ->T_REQUIRE ii);"require_once",(fun ii ->T_REQUIRE_ONCE ii);

  "use",             (fun ii -> T_USE ii);

  "class",           (fun ii -> T_CLASS ii);
  "new",             (fun ii -> T_NEW ii);
  "clone",           (fun ii -> T_CLONE ii);
  "interface",       (fun ii -> T_INTERFACE ii);
  "extends",         (fun ii -> T_EXTENDS ii);
  "implements",      (fun ii -> T_IMPLEMENTS ii);
  "instanceof",      (fun ii -> T_INSTANCEOF ii);

  "abstract", (fun ii -> T_ABSTRACT ii); "final", (fun ii -> T_FINAL ii);

  "private",         (fun ii -> T_PRIVATE ii);
  "protected",       (fun ii -> T_PROTECTED ii);
  "public",          (fun ii -> T_PUBLIC ii);

  "echo",            (fun ii -> T_ECHO ii);
  "print",           (fun ii -> T_PRINT ii);

  "eval",            (fun ii -> T_EVAL ii);

  "global",          (fun ii -> T_GLOBAL ii);
  "function",        (fun ii -> T_FUNCTION ii);
  "empty",           (fun ii -> T_EMPTY ii);
  "const",           (fun ii -> T_CONST ii);
  "var",             (fun ii -> T_VAR ii); (* was VARTOKEN *)
  "declare", (fun ii -> T_DECLARE ii); "enddeclare",(fun ii ->T_ENDDECLARE ii);
  "static",          (fun ii -> T_STATIC ii);
  "unset",           (fun ii -> T_UNSET ii);
  "isset",           (fun ii -> T_ISSET ii);
 (*e: repetitive keywords table *)

  "__halt_compiler", (fun ii -> T_HALT_COMPILER ii);

  "__class__",       (fun ii -> T_CLASS_C ii);
  "__function__", (fun ii ->T_FUNC_C ii); "__method__",(fun ii ->T_METHOD_C ii);
  "__line__",  (fun ii -> T_LINE ii); "__file__", (fun ii -> T_FILE ii);

  (* php-facebook-ext: *)
  "yield", (fun ii -> lang_ext_or_t_ident ii (fun x -> T_YIELD x));

  (* xhp: having those XHP keywords handled here could mean they can not
   * be used for entities like functions or class names. We could
   * avoid this by introducing some lexer/parser hacks so that those
   * keywords are recognized only in certain contexts (e.g. just after
   * the '{' of a class) but that complicates the full parser (note
   * also that IMHO it's better to not let the user overload those
   * special names). A simpler solution, instead of extending the lexer,
   * is to extend the grammar by having a 'ident:' rule that allows
   * the regular T_IDENT as well as those XHP tokens. See parser_php.mly.
   *)
  "attribute",  (fun ii -> xhp_or_t_ident ii (fun x -> T_XHP_ATTRIBUTE x));
  "children",  (fun ii -> xhp_or_t_ident ii (fun x -> T_XHP_CHILDREN x));
  "category",  (fun ii -> xhp_or_t_ident ii (fun x -> T_XHP_CATEGORY x));

  (* for attribute declarations *)
  "enum", (fun ii -> xhp_or_t_ident ii (fun x -> T_XHP_ENUM x));

  (* for children declarations *)
  "any", (fun ii -> xhp_or_t_ident ii (fun x -> T_XHP_ANY x));
  (* "empty" is already a PHP keyword, see T_EMPTY *)
  "pcdata", (fun ii -> xhp_or_t_ident ii (fun x -> T_XHP_PCDATA x));
]
let _ = assert ((Common.hkeys keyword_table) +> 
                 List.for_all (fun s -> s = lowercase s))
(*e: keywords_table hash *)

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
(*s: type state_mode *)
(* In most languages the lexer has no state and all strings are always
 * encoded in the same way, in the same token, wherever the string is
 * located in the file (except for strings inside comments). In PHP
 * some characters, e.g. "'", as in "I don't like you" or "'foo'" can
 * mean different things. Indeed the PHP language in fact supports
 * multiple languages or "modes" inside the same script (which also
 * make emacs mode for such language harder to define).
 * 
 * Inside the PHP script code part, the quote is the start of a string
 * and there must be a corresponding quote ending the string. Inside
 * the HTML part of a PHP file it's just a character like any other
 * character. Inside heredocs (the '<<<XXX' construct) it is again
 * considered as any other character. In the same way some strings such
 * as 'if' can again mean different things; when they are preceded by a
 * '->' they correspond to the possible name of a field, otherwise
 * they are special PHP keywords.
 * 
 * Because all of this, the lexer has multiple states which are
 * represented below and adjusted via some push/pop_mode function
 * below. Depending on the state the lexer behaves differently.
 *)

type state_mode = 
  (* aka HTML mode *)
  | INITIAL
  (* started with <?php or <?, finished by ?> *)
  | ST_IN_SCRIPTING
  (* started with <?=, finished by ?> *)
  | ST_IN_SCRIPTING2
  (* handled by using ocamllex ability to define multiple lexers
   * | ST_COMMENT
   * | ST_DOC_COMMENT
   * | ST_ONE_LINE_COMMENT
   *)
  (* started with ", finished with ". In most languages strings 
   * are a single tokens but PHP allow interpolation which means 
   * a string can contain nested PHP variables or expressions.
   *)
  | ST_DOUBLE_QUOTES
  (* started with "`", finished with "`" *)
  | ST_BACKQUOTE
  (* started with ->, finished after reading one fieldname *)
  | ST_LOOKING_FOR_PROPERTY
  (* started with ${ *)
  | ST_LOOKING_FOR_VARNAME
  (* started with $xxx[ *)
  | ST_VAR_OFFSET
  (* started with <<<XXX, finished by XXX; *)
  | ST_START_HEREDOC of string
  (* started with <<<'XXX', finished by XXX; *)
  | ST_START_NOWDOC of string

  (* started with <xx when preceded by a certain token (e.g. 'return' '<xx'), 
   * finished by '>' by transiting to ST_IN_XHP_TEXT, or really finished
   * by '/>'.
   *)
  | ST_IN_XHP_TAG of Ast_php.xhp_tag (* the current tag, e,g, ["x";"frag"] *)
  (* started with the '>' of an opening tag, finished when '</x>' *)
  | ST_IN_XHP_TEXT of Ast_php.xhp_tag (* the current tag *)

(*e: type state_mode *)

(*s: lexer state trick helpers *)
(*s: lexer state global variables *)
let default_state = INITIAL

let _mode_stack = 
  ref [default_state]
(*x: lexer state global variables *)
(* because ocamllex does not have the yyless feature, have to cheat.
 * update: in fact can hack my own yyless so maybe should revisit 
 * this code.
 *)
let _pending_tokens = 
  ref ([]: Parser_php.token list)

(* The logic to modify _last_non_whitespace_like_token is in the 
 * caller of the lexer, that is in Parse_php.tokens.
 * Used for XHP.
 *)
let _last_non_whitespace_like_token = 
  ref (None: Parser_php.token option)
(*e: lexer state global variables *)
(*s: lexer state global reinitializer *)
let reset () = 
  _mode_stack := [default_state];
  (*s: auxillary reset lexing actions *)
    _pending_tokens := [];
  (*e: auxillary reset lexing actions *)
   _last_non_whitespace_like_token := None;
  ()
(*e: lexer state global reinitializer *)

(*s: lexer state function hepers *)
let rec current_mode () = 
  try 
    Common.top !_mode_stack
  with Failure("hd") -> 
    pr2("LEXER: mode_stack is empty, defaulting to INITIAL");
    reset();
    current_mode ()
(*x: lexer state function hepers *)
let push_mode mode = Common.push2 mode _mode_stack
let pop_mode () = ignore(Common.pop2 _mode_stack)

(* What is the semantic of BEGIN() in flex ? start from scratch with empty 
 * stack ? 
 *)
let set_mode mode = 
  pop_mode();
  push_mode mode;
  ()

(* Here is an example of state transition. Given a php file like:
 *
 *   <?php return <x>foo<y>bar</y></x>; ?> 
 * 
 * we start with the stack in [INITIAL]. The transitions are then:
 * 
 * '<?php'  -> [IN_SCRIPTING], via set_mode()
 * ' '      -> [IN_SCRIPTING]
 * 'return' -> [IN_SCRIPTING]
 * '<x'     -> [IN_XHP_TAG "x"; IN_SCRIPTING], via push_mode()
 * '>'      -> [IN_XHP_TEXT "x"; IN_SCRIPTING], via set_mode()
 * 'foo'    -> [IN_XHP_TEXT "x"; IN_SCRIPTING]
 * '<y'     -> [IN_XHP_TAG "y";IN_XHP_TEXT "x"; IN_SCRIPTING], via push_mode()
 * '>'      -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x";IN_SCRIPTING], via set_mode()
 * 'bar'    -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x"; IN_SCRIPTING]
 * '</y>'   -> [IN_XHP_TEXT "x"; IN_SCRIPTING], via pop_mode()
 * '</x>'   -> [IN_SCRIPTING], via pop_mode()
 * ';'      -> [IN_SCRIPTING]
 * ' '      -> [IN_SCRIPTING]
 * '?>      -> [INITIAL], via set_mode()
 * 
 *)

(*x: lexer state function hepers *)
let push_token tok = 
  _pending_tokens := tok::!_pending_tokens
(*e: lexer state function hepers *)
(*e: lexer state trick helpers *)

(* xhp: the function below is used to disambiguate the use 
 * of ":" and "%" as either a naming (unary) operator or binary operator. 
 * 
 * How to know the following lists of tokens is correct ? 
 * We should compute FOLLOW(tok) for  all tokens and check 
 * if "%" or ":" can be in it ?
 *)
let is_in_binary_operator_position last_tok = 
  match last_tok with
  | Some (
      (* if we are after a number or any kind of scalar, then it's ok to 
       * have a binary operator *)
        T_LNUMBER _ | T_DNUMBER _
      | T_CONSTANT_ENCAPSED_STRING _ | TGUIL _ | TBACKQUOTE _ 
      (* same for ']' or ')'; anything that "terminates" an expression *)
      | TCBRA _ | TCPAR _

      | T_IDENT _ | T_VARIABLE _
    )
      -> true
  | _ -> false


}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
(*s: regexp aliases *)
let ANY_CHAR = (_ | ['\n'] )
(*x: regexp aliases *)
(* \x7f-\xff ???*)
let WHITESPACE = [' ' '\n' '\r' '\t']+
let TABS_AND_SPACES = [' ''\t']*
let NEWLINE = ("\r"|"\n"|"\r\n")
let WHITESPACEOPT = [' ' '\n' '\r' '\t']*
(*x: regexp aliases *)
let LABEL =	['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
(*x: regexp aliases *)
let LNUM =	['0'-'9']+
let DNUM =	(['0'-'9']*['.']['0'-'9']+) | (['0'-'9']+['.']['0'-'9']* )

let EXPONENT_DNUM =	((LNUM|DNUM)['e''E']['+''-']?LNUM)
let HNUM =	"0x"['0'-'9''a'-'f''A'-'F']+
(*x: regexp aliases *)
(*/*
 * LITERAL_DOLLAR matches unescaped $ that aren't followed by a label character
 * or a { and therefore will be taken literally. The case of literal $ before
 * a variable or "${" is handled in a rule for each string type
 * 
 * TODO: \x7f-\xff
 */
 *)
let DOUBLE_QUOTES_LITERAL_DOLLAR = 
  ("$"+([^'a'-'z''A'-'Z''_''$''"''\\' '{']|('\\' ANY_CHAR)))
let BACKQUOTE_LITERAL_DOLLAR =
  ("$"+([^'a'-'z''A'-'Z''_''$''`''\\' '{']|('\\' ANY_CHAR)))
(*x: regexp aliases *)
(*/*
 * CHARS matches everything up to a variable or "{$"
 * {'s are matched as long as they aren't followed by a $
 * The case of { before "{$" is handled in a rule for each string type
 *
 * For heredocs, matching continues across/after newlines if/when it's known
 * that the next line doesn't contain a possible ending label
 */
 *)
let DOUBLE_QUOTES_CHARS = 
  ("{"*([^'$''"''\\''{']| 
    ("\\" ANY_CHAR))| DOUBLE_QUOTES_LITERAL_DOLLAR)
let BACKQUOTE_CHARS = 
  ("{"*([^'$' '`' '\\' '{']|('\\' ANY_CHAR))| BACKQUOTE_LITERAL_DOLLAR)
(*x: regexp aliases *)
let HEREDOC_LITERAL_DOLLAR =
  ("$"+([^'a'-'z''A'-'Z''_''$''\n' '\r' '\\' '{' ]|('\\'[^'\n' '\r' ])))

(*/*
 * Usually, HEREDOC_NEWLINE will just function like a simple NEWLINE, but some
 * special cases need to be handled. HEREDOC_CHARS doesn't allow a line to
 * match when { or $, and/or \ is at the end. (("{"*|"$"* )"\\"?) handles that,
 * along with cases where { or $, and/or \ is the ONLY thing on a line
 *
 * The other case is when a line contains a label, followed by ONLY
 * { or $, and/or \  Handled by ({LABEL}";"?((("{"+|"$"+)"\\"?)|"\\"))
 */
 *)
let HEREDOC_NEWLINE = 
  (((LABEL";"?((("{"+|"$"+)'\\'?)|'\\'))|(("{"*|"$"*)'\\'?))NEWLINE)


(*/*
 * This pattern is just used in the next 2 for matching { or literal $, and/or
 * \ escape sequence immediately at the beginning of a line or after a label
 */
 *)
let HEREDOC_CURLY_OR_ESCAPE_OR_DOLLAR =
  (("{"+[^'$' '\n' '\r' '\\' '{'])|("{"*'\\'[^'\n' '\r'])|
   HEREDOC_LITERAL_DOLLAR)

(*/*
 * These 2 label-related patterns allow HEREDOC_CHARS to continue "regular"
 * matching after a newline that starts with either a non-label character or a
 * label that isn't followed by a newline. Like HEREDOC_CHARS, they won't match
 * a variable or "{$"  Matching a newline, and possibly label, up TO a variable
 * or "{$", is handled in the heredoc rules
 *
 * The HEREDOC_LABEL_NO_NEWLINE pattern (";"[^$\n\r\\{]) handles cases where ;
 * follows a label. [^a-zA-Z0-9_\x7f-\xff;$\n\r\\{] is needed to prevent a label
 * character or ; from matching on a possible (real) ending label
 */*)
let HEREDOC_NON_LABEL =
  ([^'a'-'z''A'-'Z''_' '$' '\n''\r''\\' '{']|HEREDOC_CURLY_OR_ESCAPE_OR_DOLLAR)
let HEREDOC_LABEL_NO_NEWLINE = 
 (LABEL([^'a'-'z''A'-'Z''0'-'9''_'';''$''\n' '\r' '\\' '{']|
   (";"[^'$' '\n' '\r' '\\' '{' ])|(";"? HEREDOC_CURLY_OR_ESCAPE_OR_DOLLAR)))

(*x: regexp aliases *)
let HEREDOC_CHARS =
  ("{"*([^'$' '\n' '\r' '\\' '{']|('\\'[^'\n' '\r']))| 
   HEREDOC_LITERAL_DOLLAR|(HEREDOC_NEWLINE+(HEREDOC_NON_LABEL|HEREDOC_LABEL_NO_NEWLINE)))

(*x: regexp aliases *)
let XHPLABEL =	['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''-']*

let XHPTAG = XHPLABEL (":" XHPLABEL)*

(* is there some special restrictions for xhp attributes ? *)
let XHPATTR = XHPLABEL

(*e: regexp aliases *)


(*****************************************************************************)
(* Rule in script *)
(*****************************************************************************)
(*s: rule st_in_scripting *)
rule st_in_scripting = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  (*s: comments rules *)
    | "/*" { 
        let info = tokinfo lexbuf in 
        let com = st_comment lexbuf in
        T_COMMENT(info +> tok_add_s com)
      }
    | "/**/" { T_COMMENT(tokinfo lexbuf) }

    | "/**" { (* RESET_DOC_COMMENT(); *)
        let info = tokinfo lexbuf in 
        let com = st_comment lexbuf in
        T_DOC_COMMENT(info +> tok_add_s com)
      }
    | "#"|"//" {
        let info = tokinfo lexbuf in
        let com = st_one_line_comment lexbuf in
        T_COMMENT(info +> tok_add_s com)
      }

    (* old: | WHITESPACE { T_WHITESPACE(tokinfo lexbuf) } *)
    | [' '  '\t']+ { TSpaces(tokinfo lexbuf) }
    | ['\n' '\r']  { TNewline(tokinfo lexbuf) }

  (*e: comments rules *)

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  (*s: symbol rules *)
    | '+' { TPLUS(tokinfo lexbuf) }      | '-' { TMINUS(tokinfo lexbuf) }
    | '*' { TMUL(tokinfo lexbuf) }       | '/' { TDIV(tokinfo lexbuf) }
    | '%' { TMOD(tokinfo lexbuf) }

    | "++" { T_INC(tokinfo lexbuf) }   | "--" { T_DEC(tokinfo lexbuf) }

    | "="  { TEQ(tokinfo lexbuf) }

    (*s: repetitive symbol rules *)
      | "+="  { T_PLUS_EQUAL(tokinfo lexbuf) }
      | "-="  { T_MINUS_EQUAL(tokinfo lexbuf) }
      | "*="  { T_MUL_EQUAL(tokinfo lexbuf) }
      | "/="  { T_DIV_EQUAL(tokinfo lexbuf) }
      | "%="  { T_MOD_EQUAL(tokinfo lexbuf) }
      | "&="  { T_AND_EQUAL(tokinfo lexbuf) }
      | "|="  { T_OR_EQUAL(tokinfo lexbuf) }
      | "^="  { T_XOR_EQUAL(tokinfo lexbuf) }
      | "<<=" { T_SL_EQUAL(tokinfo lexbuf) }
      | ">>=" { T_SR_EQUAL(tokinfo lexbuf) }
      | ".="  { T_CONCAT_EQUAL(tokinfo lexbuf) }

      | "=="  { T_IS_EQUAL(tokinfo lexbuf) }  
      | "!="  { T_IS_NOT_EQUAL(tokinfo lexbuf) }
      | "===" { T_IS_IDENTICAL(tokinfo lexbuf) }  
      | "!==" { T_IS_NOT_IDENTICAL(tokinfo lexbuf) }
      | "<>"  { T_IS_NOT_EQUAL(tokinfo lexbuf) }

      | "<=" { T_IS_SMALLER_OR_EQUAL(tokinfo lexbuf) }  
      | ">=" { T_IS_GREATER_OR_EQUAL(tokinfo lexbuf) } 

      | "<"  { TSMALLER(tokinfo lexbuf) } 
      | ">"  { TGREATER(tokinfo lexbuf) } 

      | "&&" { T_BOOLEAN_AND(tokinfo lexbuf) }     
      | "||" { T_BOOLEAN_OR(tokinfo lexbuf) } 

      | "<<" { T_SL(tokinfo lexbuf) }  
      | ">>" { T_SR(tokinfo lexbuf) }
      | "&"  { TAND(tokinfo lexbuf) }     
      | "|"  { TOR(tokinfo lexbuf) }
      | "^"  { TXOR(tokinfo lexbuf) }

      | "OR"  { T_LOGICAL_OR(tokinfo lexbuf) }
      | "AND" { T_LOGICAL_AND(tokinfo lexbuf) }
      | "XOR" { T_LOGICAL_XOR(tokinfo lexbuf) }

      | "or"  { T_LOGICAL_OR(tokinfo lexbuf) }
      | "and" { T_LOGICAL_AND(tokinfo lexbuf) }
      | "xor" { T_LOGICAL_XOR(tokinfo lexbuf) }
    (*e: repetitive symbol rules *)
  (*x: symbol rules *)
   (* Flex/Bison allow to use single characters directly as-is in the grammar 
    * by adding this in the lexer: 
    * 
    *       <ST_IN_SCRIPTING>{TOKENS} { return yytext[0];} 
    * 
    * We don't, so we have transformed all those tokens in proper tokens with
    * a name in the parser, and return them in the lexer.
    *)

    | '.'  { TDOT(tokinfo lexbuf) }
    | ','  { TCOMMA(tokinfo lexbuf) }
    | '@'  { T__AT(tokinfo lexbuf) }

    | "=>" { T_DOUBLE_ARROW(tokinfo lexbuf) }
    | "~"  { TTILDE(tokinfo lexbuf) }
    | ";"  { TSEMICOLON(tokinfo lexbuf) }
    | "!"  { TBANG(tokinfo lexbuf) }
    | "::" { TCOLCOL (tokinfo lexbuf) } (* was called T_PAAMAYIM_NEKUDOTAYIM *)

    | '(' { TOPAR(tokinfo lexbuf) }  | ')' { TCPAR(tokinfo lexbuf) }
    | '[' { TOBRA(tokinfo lexbuf) }  | ']' { TCBRA(tokinfo lexbuf) }

    | ":" { TCOLON(tokinfo lexbuf) }
    | "?" { TQUESTION(tokinfo lexbuf) }
    (* semantic grep *)
    | "..." { TDOTS(tokinfo lexbuf) }

  (*x: symbol rules *)
    (* we may come from a st_looking_for_xxx context, like in string
     * interpolation, so seeing a } we pop_mode!
     *)
    | '}' { 
        pop_mode (); 
        (* RESET_DOC_COMMENT(); ??? *)
        TCBRACE(tokinfo lexbuf)
      }
    | '{' { 
        push_mode ST_IN_SCRIPTING; 
        TOBRACE(tokinfo lexbuf)
      }
  (*x: symbol rules *)
    | ("->" as sym) (WHITESPACEOPT as white) (LABEL as label) {
     (* TODO: The ST_LOOKING_FOR_PROPERTY state does not work for now because  
      * it requires a yyless(1) which is not available in ocamllex (or is it ?)
      * So have to cheat and use instead the pending_token with push_token.
      * 
      * buggy: push_mode ST_LOOKING_FOR_PROPERTY; 
      *)
        let info = tokinfo lexbuf in
        
        let syminfo = rewrap_str sym info in

        let parse_info = Ast.parse_info_of_info info in
        let pos_after_sym   = 
          parse_info.Parse_info.charpos + String.length sym in
        let pos_after_white = pos_after_sym + String.length white in

        let whiteinfo = Parse_info.tokinfo_str_pos white pos_after_sym in
        let lblinfo = Parse_info.tokinfo_str_pos label pos_after_white in
        
        push_token (T_IDENT (label, lblinfo));
       (* todo: could be newline ... *)
        push_token (TSpaces (whiteinfo));

        T_OBJECT_OPERATOR(syminfo)
      }
    | "->" {
        T_OBJECT_OPERATOR(tokinfo lexbuf)
      }
  (*x: symbol rules *)
    (* see also T_VARIABLE below. lex use longest matching strings so this
     * rule is used only in a last resort, for code such as $$x, ${, etc
     *)
    | "$" { TDOLLAR(tokinfo lexbuf) }

  (*x: symbol rules *)
   (* XHP "elements".
    *  
    * In XHP the ":" and "%" characters are used to identify
    * XHP tags, e.g. :x:frag. There is some possible ambiguity though  
    * with their others use in PHP: ternary expr and cases for ":" and
    * the modulo binary operator for "%". It is legal in PHP to do
    * e?1:null; or case 1:null. We thus can not blindly considerate ':null'
    * as a single token. Fortunately it's not too hard
    * to disambiguate by looking at the token before and see if ":" or "%"
    * is used as a unary or binary operator.
    *
    * An alternative would be to return the same token in both cases
    * (TCOLON) and let the grammar disambiguate and build XHP tags
    * from multiple tokens (e.g. [TCOLON ":"; T_IDENT "x"; TCOLON ":";
    * TIDENT "frag"]). But this would force in the grammar to check
    * if there is no space between those tokens. This would also add
    * extra rules for things that really should be more handled at a
    * lexical level.
    *)
    | ":" (XHPTAG as tag) {
        if !Flag.xhp_builtin && 
          not (is_in_binary_operator_position !_last_non_whitespace_like_token)
        then
          let xs = Common.split ":" tag in
          T_XHP_COLONID_DEF (xs, tokinfo lexbuf) 
        else begin
          yyless (String.length tag) lexbuf;
          TCOLON(tokinfo lexbuf)
        end
      }

    | "%" (XHPTAG as tag) {
        if !Flag.xhp_builtin &&
          not (is_in_binary_operator_position !_last_non_whitespace_like_token)
        then
          let xs = Common.split ":" tag in
          T_XHP_PERCENTID_DEF (xs, tokinfo lexbuf) 
        else begin
          yyless (String.length tag) lexbuf;
          TMOD(tokinfo lexbuf)
        end
      }

     (* xhp: we need to disambiguate the different use of '<' to know whether 
      * we are in a position where an XHP construct can be started. Knowing
      * what was the previous token seems enough; no need to hack the
      * grammar to have a global shared by the lexer and parser.
      * 
      * We could maybe even return a TSMALLER in both cases and still
      * not generate any conflict in the grammar, but it feels cleaner to 
      * generate a different token, because we will really change the lexing
      * mode when we will see a '>' which makes the parser enter in the
      * ST_IN_XHP_TEXT state where it's ok to write "I don't like you"
      * in which the quote does not need to be ended.
      * 
      * note: no leading ":" for the tag when in "use" position.
      *)
      | "<" (XHPTAG as tag) { 

         let xs = Common.split ":" tag in

          match !_last_non_whitespace_like_token with
          (* todo? How to compute the correct list of tokens that
           * are possibly before a XHP construct ? trial-and-error ?
           * Usually having a '<' after a punctuation means XHP.
           * Indeed '<' is a binary operator which excepts scalar.
           * 
           * todo? TCPAR ? no, because it's ok to do (1) < (2) !
           *)
          | Some (
                TOPAR _ | TCPAR _
              | T_ECHO _ | T_PRINT _ | T_CLONE _
              | TSEMICOLON _ | TCOMMA _
              | TOBRACE _ | TCBRACE _
              | T_RETURN _ 
              | TEQ _ | T_CONCAT_EQUAL _
              | T_DOUBLE_ARROW _
              | TQUESTION _ | TCOLON _
            )
          | None (* when in sgrep/spatch mode, < is the first token *)
            when !Flag.xhp_builtin ->
              push_mode (ST_IN_XHP_TAG xs);
              T_XHP_OPEN_TAG(xs, tokinfo lexbuf)
          | _ -> 
              yyless (String.length tag) lexbuf;
              TSMALLER(tokinfo lexbuf)
        }

    | "@required" {
         let s = tok lexbuf in
         if !Flag.xhp_builtin
         then T_XHP_REQUIRED (tokinfo lexbuf) 
         else begin 
           yyless (String.length s - 1) lexbuf;
           T__AT(tokinfo lexbuf)
         end
      }

  (*e: symbol rules *)

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
    (* ugly: 'self' and 'parent' should be keywords forbidden to be used
     * as regular identifiers. But PHP is case insensitive and does not
     * consider self/parent or SELF/PARENT as keywords. I think it's
     * bad so I now consider self/parent as keywords, but still allow
     * at least the uppercase form to be used as identifier, hence those
     * two rules below.
     *)
    | "SELF"   { T_IDENT (tok lexbuf, tokinfo lexbuf) }
    | "PARENT" { T_IDENT (tok lexbuf, tokinfo lexbuf) }
  (*s: keyword and ident rules *)
    | LABEL
        { let info = tokinfo lexbuf in
          let s = tok lexbuf in
          match Common.optionise (fun () -> 
            (* PHP is case insensitive ... it's ok to write IF(...) { ... } *)
            Hashtbl.find keyword_table (String.lowercase s))
          with
          | Some f -> f info
          (* was called T_STRING in original grammar *)
          | None -> T_IDENT (s, info) 
        }

    (* Could put a special rule for "$this", but there are multiple places here
     * where we can generate a T_VARIABLE, and we can have even expressions
     * like ${this}, so it is simpler to do the "this-analysis" in the grammar, 
     * later when we generate a Var or This.
     *)
    | "$" (LABEL as s) { T_VARIABLE(s, tokinfo lexbuf) }

  (*e: keyword and ident rules *)

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  (*s: constant rules *)
    | LNUM 
        { 
          (* more? cf original lexer *)
          let s = tok lexbuf in
          let ii = tokinfo lexbuf in
          try 
            let _ = int_of_string s in
            T_LNUMBER(s, ii)
          with Failure _ -> 
            T_DNUMBER(s, (*float_of_string s,*) ii)

        }
    | HNUM 
        {
          (* more? cf orginal lexer *)
          T_DNUMBER(tok lexbuf, tokinfo lexbuf)

        }

    | DNUM|EXPONENT_DNUM { T_DNUMBER(tok lexbuf, tokinfo lexbuf) }

  (*e: constant rules *)

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  (*s: strings rules *)
    (*
     * The original PHP lexer does a few things to make the 
     * difference at parsing time between static strings (which do not 
     * contain any interpolation) and dynamic strings. So some regexps
     * below are quite hard to understand ... but apparently it works.
     * When the lexer thinks it's a dynamic strings, it let the grammar
     * do most of the hard work. See the rules using TGUIL in the grammar
     * (and here in the lexer).
     * 
     * The optional  'b' at the beginning is for binary strings.
     * 
     * /*
     *   ("{"*|"$"* ) handles { or $ at the end of a string (or the entire
     *  contents)
     * 
     * 
     * int bprefix = (yytext[0] != '"') ? 1 : 0;
     * zend_scan_escape_string(zendlval, yytext+bprefix+1, yyleng-bprefix-2, '"' TSRMLS_CC);
     */ 
     *)
  
    (* static strings *)
    | 'b'? (['"'] ((DOUBLE_QUOTES_CHARS* ("{"*|"$"* )) as s) ['"']) 
        { T_CONSTANT_ENCAPSED_STRING(s, tokinfo lexbuf) }

    | 'b'? (['\''] (([^'\'' '\\']|('\\' ANY_CHAR))* as s)  ['\''])
        { 
          (* more? cf original lexer *)
          T_CONSTANT_ENCAPSED_STRING(s, tokinfo lexbuf)
        }
  (*x: strings rules *)
    (* dynamic strings *)
    | ['"'] { 
        push_mode ST_DOUBLE_QUOTES;
        TGUIL(tokinfo lexbuf)
      }
    | ['`'] { 
        push_mode ST_BACKQUOTE;
        TBACKQUOTE(tokinfo lexbuf)
      }
  (*x: strings rules *)
    | 'b'? "<<<" TABS_AND_SPACES (LABEL as s) NEWLINE {
        set_mode (ST_START_HEREDOC s);
        T_START_HEREDOC (tokinfo lexbuf)
      }

    | 'b'? "<<<" TABS_AND_SPACES "'" (LABEL as s) "'" NEWLINE {
        set_mode (ST_START_NOWDOC s);
        (* could use another token, but simpler to reuse *)
        T_START_HEREDOC (tokinfo lexbuf)
      }

  (*e: strings rules *)

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)
  (*s: misc rules *)
    (* ugly, they could have done that in the grammar ... or maybe it was 
     * because it could lead to some ambiguities ? 
     *)
    | "(" TABS_AND_SPACES ("int"|"integer") TABS_AND_SPACES ")" 
        { T_INT_CAST(tokinfo lexbuf) }

    | "(" TABS_AND_SPACES ("real"|"double"|"float") TABS_AND_SPACES ")" 
        { T_DOUBLE_CAST(tokinfo lexbuf) }

    | "(" TABS_AND_SPACES "string" TABS_AND_SPACES ")" 
        { T_STRING_CAST(tokinfo lexbuf); }

    | "(" TABS_AND_SPACES "binary" TABS_AND_SPACES ")" 
        { T_STRING_CAST(tokinfo lexbuf); }

    | "(" TABS_AND_SPACES "array" TABS_AND_SPACES ")" 
        { T_ARRAY_CAST(tokinfo lexbuf); }

    | "(" TABS_AND_SPACES "object" TABS_AND_SPACES ")" 
        { T_OBJECT_CAST(tokinfo lexbuf); }

    | "(" TABS_AND_SPACES ("bool"|"boolean") TABS_AND_SPACES ")" 
        { T_BOOL_CAST(tokinfo lexbuf); }

    (* PHP is case insensitive for many things *)
    | "(" TABS_AND_SPACES "Array" TABS_AND_SPACES ")" 
        { T_ARRAY_CAST(tokinfo lexbuf); }
    | "(" TABS_AND_SPACES "Object" TABS_AND_SPACES ")" 
        { T_OBJECT_CAST(tokinfo lexbuf); }
    | "(" TABS_AND_SPACES ("Bool"|"Boolean") TABS_AND_SPACES ")" 
        { T_BOOL_CAST(tokinfo lexbuf); }

    | "(" TABS_AND_SPACES ("unset") TABS_AND_SPACES ")" 
        { T_UNSET_CAST(tokinfo lexbuf); }
  (*x: misc rules *)
    | "?>" 
        {
          (* because of XHP and my token merger:
           * old: | "</script"WHITESPACE*">")NEWLINE? 
           *  see tests/xhp/pb_cant_merge2.php
          *)
          match current_mode () with
          | ST_IN_SCRIPTING ->
              set_mode INITIAL;
              (*/* implicit ';' at php-end tag */*)
              (* todo? ugly, could instead generate a FakeToken or
               * ExpandedToken, but then some code later may assume
               * right now that all tokens from the lexer are 
               * origin tokens, so may be hard to change.
               * 
               * old: (T_CLOSE_TAG(tokinfo lexbuf))
               * note that T_CLOSE_TAG was skipped anyway in Parse_php.parse_php
               *)
              TSEMICOLON(tokinfo lexbuf)
              
          | ST_IN_SCRIPTING2 ->
              set_mode INITIAL;
              T_CLOSE_TAG_OF_ECHO(tokinfo lexbuf)
          | _ ->
              raise Impossible
        }

  (*e: misc rules *)

  (* ----------------------------------------------------------------------- *)
  (*s: semi repetitive st_in_scripting rules for eof and error handling *)
    | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
    | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }
  (*e: semi repetitive st_in_scripting rules for eof and error handling *)

(*e: rule st_in_scripting *)

(*****************************************************************************)
(* Rule initial (html) *)
(*****************************************************************************)
(*s: rule initial *)
and initial = parse

  | "<?php"([' ''\t']|NEWLINE) 
      { set_mode ST_IN_SCRIPTING;
        T_OPEN_TAG(tokinfo lexbuf)
      }

  | "<?PHP"([' ''\t']|NEWLINE) 
  | "<?Php"([' ''\t']|NEWLINE) 
      { 
        (* "BAD USE OF <PHP at initial state, replace by <?php"; *)
        set_mode ST_IN_SCRIPTING;
        T_OPEN_TAG(tokinfo lexbuf)
      }

  | (([^'<']|"<"[^'?''%''s''<'])+(*{1,400}*))|"<s"|"<" {
      (* more? cf orinal lexer  *)
    T_INLINE_HTML(tok lexbuf, tokinfo lexbuf)
    }

  | "<?=" { 
      (* less: if short_tags normally, otherwise T_INLINE_HTML *)
      set_mode ST_IN_SCRIPTING2;
      (* todo? ugly, may be better ot generate a real T_ECHO token
       * with maybe a FakeTok or ExpandeTok.
       *)
      T_OPEN_TAG_WITH_ECHO(tokinfo lexbuf);
    }

  | "<?" | "<script" WHITESPACE+ "language" WHITESPACE* "=" WHITESPACE * 
           ("php"|"\"php\""|"\'php\'") WHITESPACE*">" 
     {
       (* XXX if short_tags normally otherwise T_INLINE_HTML *)
       (* pr2 "BAD USE OF <? at initial state, replace by <?php"; *)
       set_mode ST_IN_SCRIPTING;
       T_OPEN_TAG(tokinfo lexbuf);
     }

  (*------------------------------------------------------------------------ *)
      
  | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
  | _ (* ANY_CHAR *) { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }


(*e: rule initial *)

(*****************************************************************************)
(* Rule looking_for_xxx *)
(*****************************************************************************)
(*s: rule st_looking_for_property *)

(* TODO not used for now *)
and st_looking_for_property = parse
  | "->" { T_OBJECT_OPERATOR(tokinfo lexbuf) }
  | LABEL {
      pop_mode();
      T_IDENT(tok lexbuf, tokinfo lexbuf)
    }
(*
  | ANY_CHAR {
      (* XXX yyless(0) ?? *)
      pop_mode();
    }
*)

(*e: rule st_looking_for_property *)

(*****************************************************************************)
(*s: rule st_looking_for_varname *)
and st_looking_for_varname = parse
  | LABEL {
      set_mode ST_IN_SCRIPTING;
      T_STRING_VARNAME(tok lexbuf, tokinfo lexbuf)
    }
  | _ {
      yyless 1 lexbuf;
      set_mode ST_IN_SCRIPTING;
      st_in_scripting lexbuf
    }
(*e: rule st_looking_for_varname *)

(*****************************************************************************)
(*s: rule st_var_offset *)
and st_var_offset = parse

  | LNUM | HNUM { (* /* Offset must be treated as a string */ *)
      T_NUM_STRING (tok lexbuf, tokinfo lexbuf)
    }

  | "$" (LABEL as s) { T_VARIABLE(s, tokinfo lexbuf) }
  | LABEL            { T_IDENT(tok lexbuf, tokinfo lexbuf)  }

  | "]" { 
      pop_mode();
      TCBRA(tokinfo lexbuf);
    }
 (*s: repetitive st_var_offset rules for error handling *)
   | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
   | _ { 
       if !Flag.verbose_lexing 
       then pr2_once ("LEXER:unrecognised symbol, in st_var_offset rule:"^tok lexbuf);
       TUnknown (tokinfo lexbuf)
     }
 (*e: repetitive st_var_offset rules for error handling *)
(*e: rule st_var_offset *)

(*****************************************************************************)
(* Rule strings *)
(*****************************************************************************)
(*s: rule st_double_quotes *)

and st_double_quotes = parse

  | DOUBLE_QUOTES_CHARS+ { 
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) 
    } 

  (* todo? was in original scanner ? *)
  | "{" {  T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)  }

  (*s: encapsulated dollar stuff rules *)
    | "$" (LABEL as s)     { T_VARIABLE(s, tokinfo lexbuf) }
  (*x: encapsulated dollar stuff rules *)
    | "$" (LABEL as s) "[" {
          let info = tokinfo lexbuf in
        
          let varinfo = rewrap_str ("$" ^ s) info in
          let charpos_info = Ast.pos_of_info varinfo in
          let pos_after_label = charpos_info + String.length ("$" ^ s) in

          let bra_info = Parse_info.tokinfo_str_pos "[" pos_after_label in
          push_token (TOBRA (bra_info));
          push_mode ST_VAR_OFFSET;
          T_VARIABLE(s, varinfo)
      }
    (* bugfix: can have strings like "$$foo$" *)
    | "$" { T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) }

  (*x: encapsulated dollar stuff rules *)
    | "{$" { 
        yyless 1 lexbuf;
        push_mode ST_IN_SCRIPTING;
        T_CURLY_OPEN(tokinfo lexbuf);
      }
  (*x: encapsulated dollar stuff rules *)
    | "${" {
        push_mode ST_LOOKING_FOR_VARNAME;
        T_DOLLAR_OPEN_CURLY_BRACES(tokinfo lexbuf);
      }
  (*e: encapsulated dollar stuff rules *)

  | ['"'] { 
      (* was originally set_mode ST_IN_SCRIPTING, but with XHP
       * the context for a double quote may not be anymore always
       * ST_IN_SCRIPTING
       *)
      pop_mode ();
      TGUIL(tokinfo lexbuf)
    }
 (*s: repetitive st_double_quotes rules for error handling *)
   | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
   | _ { 
       if !Flag.verbose_lexing 
       then pr2_once ("LEXER:unrecognised symbol, in st_double_quotes rule:"^tok lexbuf);
       TUnknown (tokinfo lexbuf)
     }
 (*e: repetitive st_double_quotes rules for error handling *)
(*e: rule st_double_quotes *)

(* ----------------------------------------------------------------------- *)
(*s: rule st_backquote *)
(* mostly copy paste of st_double_quotes; just the end regexp is different *)
and st_backquote = parse 

  | BACKQUOTE_CHARS+ { 
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) 
    }

  (*s: encapsulated dollar stuff rules *)
    | "$" (LABEL as s)     { T_VARIABLE(s, tokinfo lexbuf) }
  (*x: encapsulated dollar stuff rules *)
    | "$" (LABEL as s) "[" {
          let info = tokinfo lexbuf in
        
          let varinfo = rewrap_str ("$" ^ s) info in
          let charpos_info = Ast.pos_of_info varinfo in
          let pos_after_label = charpos_info + String.length ("$" ^ s) in

          let bra_info = Parse_info.tokinfo_str_pos "[" pos_after_label in
          push_token (TOBRA (bra_info));
          push_mode ST_VAR_OFFSET;
          T_VARIABLE(s, varinfo)
      }
    (* bugfix: can have strings like "$$foo$" *)
    | "$" { T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) }

  (*x: encapsulated dollar stuff rules *)
    | "{$" { 
        yyless 1 lexbuf;
        push_mode ST_IN_SCRIPTING;
        T_CURLY_OPEN(tokinfo lexbuf);
      }
  (*x: encapsulated dollar stuff rules *)
    | "${" {
        push_mode ST_LOOKING_FOR_VARNAME;
        T_DOLLAR_OPEN_CURLY_BRACES(tokinfo lexbuf);
      }
  (*e: encapsulated dollar stuff rules *)

  | ['`'] { 
      set_mode ST_IN_SCRIPTING;
      TBACKQUOTE(tokinfo lexbuf) 
    } 

  (*s: repetitive st_backquote rules for error handling *)
    | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
    | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in st_backquote rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }
  (*e: repetitive st_backquote rules for error handling *)
(*e: rule st_backquote *)

(* ----------------------------------------------------------------------- *)
(*s: rule st_start_heredoc *)
(* as heredoc have some of the semantic of double quote strings, again some
 * rules from st_double_quotes are copy pasted here.
 *)
and st_start_heredoc stopdoc = parse

  | (LABEL as s) (";"? as semi) (['\n' '\r'] as space) {
      let info = tokinfo lexbuf in 

      let lbl_info = rewrap_str s info in

      let pos = Ast.pos_of_info info in
      let pos_after_label = pos + String.length s in
      let pos_after_semi = pos_after_label + String.length semi in

      let colon_info = 
        Parse_info.tokinfo_str_pos semi pos_after_label in
      let space_info = 
        Parse_info.tokinfo_str_pos (string_of_char space) pos_after_semi in
      
      if s = stopdoc
      then begin
        set_mode ST_IN_SCRIPTING;
        push_token (TNewline (space_info));
        if semi = ";"
        then push_token (TSEMICOLON (colon_info));
          
        T_END_HEREDOC(lbl_info)
      end else 
        T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }
  (* | ANY_CHAR { set_mode ST_HERE_DOC; yymore() ??? } *)

  (*s: encapsulated dollar stuff rules *)
    | "$" (LABEL as s)     { T_VARIABLE(s, tokinfo lexbuf) }
  (*x: encapsulated dollar stuff rules *)
    | "$" (LABEL as s) "[" {
          let info = tokinfo lexbuf in
        
          let varinfo = rewrap_str ("$" ^ s) info in
          let charpos_info = Ast.pos_of_info varinfo in
          let pos_after_label = charpos_info + String.length ("$" ^ s) in

          let bra_info = Parse_info.tokinfo_str_pos "[" pos_after_label in
          push_token (TOBRA (bra_info));
          push_mode ST_VAR_OFFSET;
          T_VARIABLE(s, varinfo)
      }
    (* bugfix: can have strings like "$$foo$" *)
    | "$" { T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) }

  (*x: encapsulated dollar stuff rules *)
    | "{$" { 
        yyless 1 lexbuf;
        push_mode ST_IN_SCRIPTING;
        T_CURLY_OPEN(tokinfo lexbuf);
      }
  (*x: encapsulated dollar stuff rules *)
    | "${" {
        push_mode ST_LOOKING_FOR_VARNAME;
        T_DOLLAR_OPEN_CURLY_BRACES(tokinfo lexbuf);
      }
  (*e: encapsulated dollar stuff rules *)


(*/* Match everything up to and including a possible ending label, so if the label
 * doesn't match, it's kept with the rest of the string
 *
 * {HEREDOC_NEWLINE}+ handles the case of more than one newline sequence that
 * couldn't be matched with HEREDOC_CHARS, because of the following label
 */
 *)
  | ((HEREDOC_CHARS* HEREDOC_NEWLINE+) as str)
       (LABEL as s)
       (";"? as semi) (['\n' '\r'] as space) {

      let info = tokinfo lexbuf in 

      let here_info = rewrap_str str info in

      let pos = Ast.pos_of_info info in
      let pos_after_here = pos + String.length str in
      let pos_after_label = pos_after_here + String.length s in
      let pos_after_semi = pos_after_label + String.length semi in

      let lbl_info = 
        Parse_info.tokinfo_str_pos s pos_after_here in
      let colon_info = 
        Parse_info.tokinfo_str_pos semi pos_after_label in
      let space_info = 
        Parse_info.tokinfo_str_pos (string_of_char space) pos_after_semi in
      

      if s = stopdoc 
      then begin
        set_mode ST_IN_SCRIPTING;
        push_token (TNewline(space_info));
        if semi = ";"
        then push_token (TSEMICOLON (colon_info));
        push_token (T_END_HEREDOC(lbl_info));
        T_ENCAPSED_AND_WHITESPACE(str, here_info)
      end
      else begin
        T_ENCAPSED_AND_WHITESPACE (tok lexbuf, tokinfo lexbuf)
      end
    }

(*/* ({HEREDOC_NEWLINE}+({LABEL}";"?)?)? handles the possible case of newline
 * sequences, possibly followed by a label, that couldn't be matched with
 * HEREDOC_CHARS because of a following variable or "{$"
 *
 * This doesn't affect real ending labels, as they are followed by a newline,
 * which will result in a longer match for the correct rule if present
 */
 *)
  | HEREDOC_CHARS*(HEREDOC_NEWLINE+(LABEL";"?)?)? {
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }

  (*s: repetitive st_start_heredoc rules for error handling *)
    | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
    | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in st_start_heredoc rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }
  (*e: repetitive st_start_heredoc rules for error handling *)

(* ----------------------------------------------------------------------- *)
(* todo? this is not what was in the original lexer, but the original lexer
 * does complicated stuff ...
 *)
and st_start_nowdoc stopdoc = parse

  | (LABEL as s) (";"? as semi) (['\n' '\r'] as space) {
      let info = tokinfo lexbuf in 

      let lbl_info = rewrap_str s info in

      let pos = Ast.pos_of_info info in
      let pos_after_label = pos + String.length s in
      let pos_after_semi = pos_after_label + String.length semi in

      let colon_info = 
        Parse_info.tokinfo_str_pos semi pos_after_label in
      let space_info = 
        Parse_info.tokinfo_str_pos (string_of_char space) pos_after_semi in
      
      if s = stopdoc
      then begin
        set_mode ST_IN_SCRIPTING;
        push_token (TNewline (space_info));
        if semi = ";"
        then push_token (TSEMICOLON (colon_info));
        (* reuse same token than for heredocs *)
        T_END_HEREDOC(lbl_info)
      end else 
        T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }
  | [^ '\n' '\r']+ {
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }

  | ['\n' '\r'] {
      TNewline (tokinfo lexbuf)
    }

  | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
  | _ { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in st_start_nowdoc rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
    }

      
(*e: rule st_start_heredoc *)

(*****************************************************************************)
(* Rules for XHP *)
(*****************************************************************************)
(* XHP lexing states and rules *) 

and st_in_xhp_tag current_tag = parse

  (* The original XHP parser have some special handlings of 
   * whitespace and enforce to use certain whitespace at
   * certain places. Not sure I need to enforce this too.
   * Simpler to ignore whitespaces.
   * 
   * todo? allow comments too there ? 
   *)
  | [' ' '\t']+ { TSpaces(tokinfo lexbuf) }
  | ['\n' '\r'] { TNewline(tokinfo lexbuf) }

  (* attribute management *)
  | XHPATTR { T_XHP_ATTR(tok lexbuf, tokinfo lexbuf) }
  | "="     { TEQ(tokinfo lexbuf) }

  (* not sure if XHP strings needs the interpolation support *)
  | ['"'] { 
      push_mode ST_DOUBLE_QUOTES;
      TGUIL(tokinfo lexbuf)
    }
  | "{" {
      push_mode ST_IN_SCRIPTING; 
      TOBRACE(tokinfo lexbuf)
    }

  (* a singleton tag *)
  | "/>" { 
      pop_mode ();
      T_XHP_SLASH_GT (tokinfo lexbuf) 
    }

  (* When we see a ">", it means it's just the end of 
   * the opening tag. Transit to IN_XHP_TEXT.
   *)
  | ">" { 
      set_mode (ST_IN_XHP_TEXT current_tag);
      T_XHP_GT (tokinfo lexbuf)
    }

  | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
  | _  { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in XHP tag:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
    }

(* ----------------------------------------------------------------------- *)
and st_in_xhp_text current_tag = parse

  (* a nested xhp construct *)
  | "<" (XHPTAG as tag) { 
      let xs = Common.split ":" tag in

      push_mode (ST_IN_XHP_TAG xs);
      T_XHP_OPEN_TAG(xs, tokinfo lexbuf)
    }

  | "<" "/" (XHPTAG as tag) ">" { 
      let xs = Common.split ":" tag in
      if (xs <> current_tag)
      then begin
        pr2 (spf "XHP: wrong closing tag for, %s != %s"
                     (Common.join ":" xs)
                     (Common.join ":" current_tag));
      end;
      pop_mode ();
      T_XHP_CLOSE_TAG(Some xs, tokinfo lexbuf)

    }
  (* shortcut for closing tag ? *)
  | "<" "/" ">" { 
      (* no check :( *)
      pop_mode ();
      T_XHP_CLOSE_TAG(None, tokinfo lexbuf)
    }

  (* PHP interpolation. How the user can produce a { ? &;something ? *)
  | "{" {
      push_mode ST_IN_SCRIPTING; 
      TOBRACE(tokinfo lexbuf)
    }

  (* opti: *)
  | [^'<' '{']+ { T_XHP_TEXT (tok lexbuf, tokinfo lexbuf) }


  | eof { EOF (tokinfo lexbuf +> Ast.rewrap_str "") }
  | _  { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in XHP text:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)
(*s: rule st_comment *)
and st_comment = parse 
  | "*/" { tok lexbuf }

  (* noteopti: *)
  | [^'*']+ { let s = tok lexbuf in s ^ st_comment lexbuf } 
  | "*"     { let s = tok lexbuf in s ^ st_comment lexbuf }

  (*s: repetitive st_comment rules for error handling *)
    | eof { pr2 "LEXER: end of file in comment"; "*/"}
    | _  { 
        let s = tok lexbuf in
        pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ st_comment lexbuf
      }
  (*e: repetitive st_comment rules for error handling *)
(*e: rule st_comment *)

(*s: rule st_one_line_comment *)
and st_one_line_comment = parse
  | "?"|"%"|">" { let s = tok lexbuf in s ^ st_one_line_comment lexbuf }
  | ([^'\n' '\r' '?''%''>']* as start) (ANY_CHAR as x)
      { 
        (match x with
        | '?' | '%' | '>' -> 
            yyless 1 lexbuf;
            start ^ st_one_line_comment lexbuf
        (* end of recursion when new line or other character  *)
        | '\n' -> 
            (* don't want the newline to be part of the comment *)
            yyless 1 lexbuf;
            start
        | c -> start ^ String.make 1 c
        )
      }
  | NEWLINE { 
      (* don't want the newline to be part of the comment *)
      yyless 1 lexbuf;
      ""
    }
  | "?>" { 
      (* "%>" is only when use asp_tags *)
      yyless 2 lexbuf;
      ""
    }

  (*s: repetitive st_one_line_comment rules for error handling *)
    | eof { pr2 "LEXER: end of file in comment"; "*/" }
    | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in st_one_line_comment rule:"^tok lexbuf);
        tok lexbuf
      }
  (*e: repetitive st_one_line_comment rules for error handling *)
(*e: rule st_one_line_comment *)

(*e: lexer_php.mll *)
