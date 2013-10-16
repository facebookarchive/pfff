{
(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
 *  Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 *  Released under the GNU General Public License 
 * 
 * ocamllex lexer for Java
 * 
 * Attempts to conform to:
 * 
 * The Java Language Specification
 * Second Edition
 * 
 * James Gosling, Bill Joy, Guy Steele, Gilad Bracha 
 *)
open Common 

open Parser_java
module Flag = Flag_parsing_java

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

exception Lexical of string
(*exception Unterminated_comment*)

let error s =
  if !Flag.exn_when_lexical_error
  then raise (Lexical (s))
  else 
    if !Flag.verbose_lexing
    then pr2_once ("LEXER: " ^ s)
    else ()

let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
let literal v = (v, (fun ii -> LITERAL (v,ii)))
let primitive_type t = (t, (fun ii -> PRIMITIVE_TYPE (t, ii)))

let keyword_table = Common.hash_of_list [
  "abstract", (fun ii -> ABSTRACT ii);
  "boolean", (fun ii -> BOOLEAN ii);
  "break", (fun ii -> BREAK ii);
  "byte", (fun ii -> BYTE ii);
  "case", (fun ii -> CASE ii);
  "catch", (fun ii -> CATCH ii);
  "char", (fun ii -> CHAR ii);
  "class", (fun ii -> CLASS ii);
  "const", (fun ii -> CONST ii);
  "continue", (fun ii -> CONTINUE ii);
  "default", (fun ii -> DEFAULT ii);
  "do", (fun ii -> DO ii);
  "double", (fun ii -> DOUBLE ii);
  "else", (fun ii -> ELSE ii);
  "extends", (fun ii -> EXTENDS ii);
  "final", (fun ii -> FINAL ii);
  "finally", (fun ii -> FINALLY ii);
  "float", (fun ii -> FLOAT ii);
  "for", (fun ii -> FOR ii);
  "goto", (fun ii -> GOTO ii);
  "if", (fun ii -> IF ii);
  "implements", (fun ii -> IMPLEMENTS ii);
  "import", (fun ii -> IMPORT ii);
  "instanceof", (fun ii -> INSTANCEOF ii);
  "int", (fun ii -> INT ii);
  "interface", (fun ii -> INTERFACE ii);
  "long", (fun ii -> LONG ii);
  "native", (fun ii -> NATIVE ii);
  "new", (fun ii -> NEW ii);
  "package", (fun ii -> PACKAGE ii);
  "private", (fun ii -> PRIVATE ii);
  "protected", (fun ii -> PROTECTED ii);
  "public", (fun ii -> PUBLIC ii);
  "return", (fun ii -> RETURN ii);
  "short", (fun ii -> SHORT ii);
  "static", (fun ii -> STATIC ii);
  "strictfp", (fun ii -> STRICTFP ii);
  "super", (fun ii -> SUPER ii);
  "switch", (fun ii -> SWITCH ii);
  "synchronized", (fun ii -> SYNCHRONIZED ii);
  "this", (fun ii -> THIS ii);
  "throw", (fun ii -> THROW ii);
  "throws", (fun ii -> THROWS ii);
  "transient", (fun ii -> TRANSIENT ii);
  "try", (fun ii -> TRY ii);
  "void", (fun ii -> VOID ii);
  "volatile", (fun ii -> VOLATILE ii);
  "while", (fun ii -> WHILE ii);

  literal "true";
  literal "false";
  literal "null";

  primitive_type "byte";
  primitive_type "short";
  primitive_type "char";
  primitive_type "int";
  primitive_type "long";
  primitive_type "float";
  primitive_type "double";
  primitive_type "boolean";

  (* javaext: 1.4 *)
  "assert", (fun ii -> ASSERT ii);

  (* javaext: 1.? *)
  "enum", (fun ii -> ENUM ii);

]
}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)

(* CHAPTER 3: Lexical Structure *)

(* 3.4 Line Terminators *)

let LF = '\n'  (* newline *)
let CR = '\r'  (* return *)

let LineTerminator = LF | CR | CR LF
let InputCharacter = [^ '\r' '\n']

(* 3.5 Input Elements and Tokens *)

let SUB = '\026' (* control-Z *) (* decimal *)

(* 3.6 White Space *)

let SP = ' '     (* space *)
let HT = '\t'    (* horizontal tab *)
let FF = '\012'  (* form feed *) (* decimal *)

let WhiteSpace = SP | HT | FF (* | LineTerminator -- handled separately *)

(* 3.7 Comments *)

(* let TraditionalComment = "/*" ([^ '*'] | '*' [^ '/'])* "*/" *)
let EndOfLineComment = "//" InputCharacter* LineTerminator
(* let Comment = TraditionalComment | EndOfLineComment *)

(* 3.8 Identifiers *)

let Letter = ['A'-'Z' 'a'-'z' '_' '$']
let Digit = ['0'-'9']
let Identifier = Letter (Letter | Digit)*

(* 3.10.1 Integer Literals *)

let IntegerTypeSuffix = ['l' 'L']

let DecimalIntegerLiteral = ('0' | ['1'-'9'] Digit*) IntegerTypeSuffix?

let HexDigit = ['0'-'9' 'a'-'f' 'A'-'F']
let HexIntegerLiteral = '0' ['x' 'X'] HexDigit+ IntegerTypeSuffix?

let OctalDigit = ['0'-'7']
let OctalIntegerLiteral = '0' OctalDigit+ IntegerTypeSuffix?

let IntegerLiteral =
  DecimalIntegerLiteral
| HexIntegerLiteral
| OctalIntegerLiteral

(* 3.10.2 Floating-Point Literals *)

let ExponentPart = ['e' 'E'] ['+' '-']? Digit+

let FloatTypeSuffix = ['f' 'F' 'd' 'D']

let FloatingPointLiteral =
  (Digit+ '.' Digit* | '.' Digit+) ExponentPart? FloatTypeSuffix?
| Digit+ (ExponentPart FloatTypeSuffix? | ExponentPart? FloatTypeSuffix)

(* 3.10.3 Boolean Literals *)

let BooleanLiteral = "true" | "false"

(* 3.10.6 Escape Sequences for Character and String Literals *)

let OctalEscape = '\\' ['0'-'3']? OctalDigit? OctalDigit

(* Not in spec -- added because we don't handle Unicode elsewhere. *)

let UnicodeEscape = "\\u" HexDigit HexDigit HexDigit HexDigit

let EscapeSequence =
  '\\' ['b' 't' 'n' 'f' 'r' '"' '\'' '\\']
| OctalEscape
| UnicodeEscape

(* 3.10.4 Character Literals *)

(* ugly: hardcoded stuff for fbandroid, error in SoundexTest.java *)
let UnicodeX = "�"

let SingleCharacter = [^ '\'' '\\' '\n' '\r']
let CharacterLiteral = '\'' (SingleCharacter | EscapeSequence | UnicodeX ) '\''


(* 3.10.5 String Literals *)

let StringCharacter = [^ '"' '\\' '\n' '\r']
let StringLiteral = '"' (StringCharacter | EscapeSequence)* '"'

(* 3.10.7 The Null Literal *)

let NullLiteral = "null"

(* 3.10 Literals *)

let Literal =
  IntegerLiteral
| FloatingPointLiteral
| BooleanLiteral
| CharacterLiteral
| StringLiteral
| NullLiteral

(* Assignment operators, except '=', from section 3.12 *)

let AssignmentOperator =
  ('+' | '-' | '*' | '/' | '&' | '|' | '^' | '%' | "<<" | ">>" | ">>>") '='


let newline = '\n'

(*****************************************************************************)
(* Token rule *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
(* old:
| WhiteSpace
    { token lexbuf }
| LineTerminator
    { next_line lexbuf; token lexbuf }
| "/*"
    { begin_comment lexbuf; comment lexbuf; token lexbuf }
| "//" InputCharacter* LineTerminator (* inline of EndOfLineComment*)
    { eol_comment lexbuf; next_line lexbuf; token lexbuf }
*)
 | [' ' '\t' '\r' '\011' '\012' ]+  { TCommentSpace (tokinfo lexbuf) }

 | newline { TCommentNewline (tokinfo lexbuf) }

| "/*"
    { 
      let info = tokinfo lexbuf in 
      let com = comment lexbuf in
      TComment(info +> Parse_info.tok_add_s com) 
    }
(* don't keep the trailing \n; it will be in another token *)
| "//" InputCharacter* 
   { TComment(tokinfo lexbuf) }


  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
| Identifier
    { 
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
   
      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> IDENTIFIER (s, info)
          
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

| IntegerLiteral  { TInt (tok lexbuf, tokinfo lexbuf) }
| FloatingPointLiteral { TFloat (tok lexbuf, tokinfo lexbuf) }
| CharacterLiteral { TChar (tok lexbuf, tokinfo lexbuf) }
| StringLiteral { TString (tok lexbuf, tokinfo lexbuf) }
| BooleanLiteral { LITERAL (tok lexbuf, tokinfo lexbuf) }
| NullLiteral { LITERAL (tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)

(* 3.11 Separators *)
| '('  { LP(tokinfo lexbuf) } | ')'  { RP(tokinfo lexbuf) }
| '{'  { LC(tokinfo lexbuf) } | '}'  { RC(tokinfo lexbuf) }
| '['  { LB(tokinfo lexbuf) } | ']'  { RB(tokinfo lexbuf) }
| ';'  { SM(tokinfo lexbuf) }
| ','  { CM(tokinfo lexbuf) }
| '.'  { DOT(tokinfo lexbuf) }

(* pad: to avoid some conflicts *)
| "[]"  { LB_RB(tokinfo lexbuf) }

(* 3.12 Operators *)
| "="  { EQ(tokinfo lexbuf) }
(* relational operator also now used for generics, can be transformed in LT2 *)
| "<"  { LT(tokinfo lexbuf) } | ">"  { GT(tokinfo lexbuf) }
| "!"  { NOT(tokinfo lexbuf) }
| "~"  { COMPL(tokinfo lexbuf) }
| "?"  { COND(tokinfo lexbuf) }
| ":"  { COLON(tokinfo lexbuf) }
| "=="  { EQ_EQ(tokinfo lexbuf) }
| "<="  { LE(tokinfo lexbuf) } | ">="  { GE(tokinfo lexbuf) }
| "!="  { NOT_EQ(tokinfo lexbuf) }
| "&&"  { AND_AND(tokinfo lexbuf) } | "||"  { OR_OR(tokinfo lexbuf) }
| "++"  { INCR(tokinfo lexbuf) } | "--"  { DECR(tokinfo lexbuf) }
| "+"  { PLUS(tokinfo lexbuf) } | "-"  { MINUS(tokinfo lexbuf) }
| "*"  { TIMES(tokinfo lexbuf) } | "/"  { DIV(tokinfo lexbuf) }
| "&"  { AND(tokinfo lexbuf) } | "|"  { OR(tokinfo lexbuf) }
| "^"  { XOR(tokinfo lexbuf) }
| "%"  { MOD(tokinfo lexbuf) }
| "<<"  { LS(tokinfo lexbuf) } 
(* this may be split in two tokens in fix_tokens_java.ml *)
| ">>"  { SRS(tokinfo lexbuf) }
| ">>>"  { URS(tokinfo lexbuf) }

(* ext: annotations *)
| "@" { AT(tokinfo lexbuf) }
(* ext: ?? *)
| "..."  { DOTS(tokinfo lexbuf) }

| AssignmentOperator  { 
    let info = tokinfo lexbuf in
    let s = tok lexbuf in
    OPERATOR_EQ (s, info) 
  }

| SUB? eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }

| _ { 
  error ("unrecognised symbol, in token rule:"^tok lexbuf);
  TUnknown (tokinfo lexbuf)
  }

(*****************************************************************************)
(* Comments *)
(*****************************************************************************)

(* less: allow only char-'*' ? *)
and comment = parse
  | "*/"     { tok lexbuf }
  (* noteopti: *)
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | eof  { error ("Unterminated_comment");  ""  }
  | _  {
    let s = tok lexbuf in
    error ("unrecognised symbol in comment:"^s);
    s ^ comment lexbuf
  }
(* old:
and comment = parse
  "*/" { end_comment lexbuf }
| LineTerminator  { continue_comment lexbuf; next_line lexbuf; comment lexbuf }
| eof  { raise (Lexical "Unterminated_comment") }
| _  { continue_comment lexbuf; comment lexbuf }
*)
