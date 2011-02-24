(* $Id: nethtml_scanner.mll 1219 2009-04-14 13:28:56Z ChriS $
 * ----------------------------------------------------------------------
 *
 *)

{
  type token =
      Lcomment  (* <!-- *)
    | Rcomment  (* --> *)
    | Mcomment  (* within comment *)
    | Ldoctype  (* <! *)
    | Rdoctype  (* > *)
    | Mdoctype  (* within declaration *)
    | Lpi       (* <? *)
    | Rpi       (* ?> or > *)
    | Mpi       (* within processing instruction *)
    | Lelement of string
    | Lelementend of string
    | Relement  (* > *)
    | Relement_empty   (* />, for XML compat *)
    | Cdata of string 
    | Space of int
    | Name of string
    | Is
    | Literal of string
    | Other
    | Eof
}

(* Simplified rules: Only ASCII is recognized as character set *)

let letter = ['A'-'Z' 'a'-'z' ]
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']
let namechar = letter | digit | '.' | ':' | '-' | '_'
let name = ( letter | '_' | ':' ) namechar*
let nmtoken = namechar+
let ws = [ ' ' '\t' '\r' '\n' ]
let string_literal1 = '"' [^ '"' ]* '"'
let string_literal2 = "'" [^ '\'' ]* "'"
let string_literal3 = [^ '"' '\'' '>' '=' ' ' '\t' '\n' '\r' ]+
let string_literal4 = [^ '"' '\'' '>' ' ' '\t' '\n' '\r' ]+

(* This following rules reflect HTML as it is used, not the SGML
 * rules.
 *)

rule scan_document = parse
  | "<!--"
      { Lcomment }
  | "<!"
      { Ldoctype }
  | "<?"
      { Lpi }
  | "<" name
      { let s = Lexing.lexeme lexbuf in
	Lelement (String.sub s 1 (String.length s - 1))
      }
  | "</" name
      { let s = Lexing.lexeme lexbuf in
	Lelementend (String.sub s 2 (String.length s - 2))
      }
  | "<"                (* misplaced "<" *)
      { Cdata "<" }
  | eof
      { Eof }
  | [^ '<' ]+
      { Cdata (Lexing.lexeme lexbuf)}

and scan_special = parse
  | "</" name 
      { let s = Lexing.lexeme lexbuf in
	Lelementend (String.sub s 2 (String.length s - 2))
      }
  | "<"
      { Cdata "<" }
  | eof
      { Eof }
  | [^ '<' ]+
      { Cdata (Lexing.lexeme lexbuf)}


and scan_comment = parse
  | "-->"
      { Rcomment }  (* FIXME: There may be any number of ws between -- and > *)
  | "-"
      { Mcomment }
  | eof
      { Eof }
  | [^ '-']+
      { Mcomment }

and scan_doctype = parse
  | ">"                   (* Occurence in strings, and [ ] brackets ignored *)
      { Rdoctype }
  | eof
      { Eof }
  | [^ '>' ] +
      { Mdoctype }

and scan_pi = parse
  | "?>"
      { Rpi }
  | ">"
      { Rpi }
  | eof
      { Eof }
  | '?' 
      { Mpi }
  | [^ '>' '?' ] +
      { Mpi }

and scan_element = parse
  | ">"
      { Relement }
  | "/>"
      { Relement_empty }
  | ws+
      { Space (String.length (Lexing.lexeme lexbuf)) }
  | name
      { Name (Lexing.lexeme lexbuf) }
  | "="
      { Is }
  | '"' 
      { Other }
  | "'"
      { Other }
  | string_literal3
      { Literal (Lexing.lexeme lexbuf) }
  | eof
      { Eof }
  | _
      { Other }

and scan_element_after_Is = parse
  | ">"
      { Relement }
  | "/>"
      { Relement_empty }
  | ws+
      { Space (String.length (Lexing.lexeme lexbuf)) }
  | '"' 
      { try
	  Literal (scan_string_literal1 lexbuf)
	with
	  | _ -> Other
      }
  | "'"
      { try
	  Literal (scan_string_literal2 lexbuf)
	with
	  | _ -> Other
      }
  | string_literal4
      { Literal (Lexing.lexeme lexbuf) }
  | eof
      { Eof }
  | _
      { Other }

and scan_string_literal1 = parse
  | ( [^ '"' ]* as s) '"'
      { s }

and scan_string_literal2 = parse
  | ( [^ '\'' ]* as s) '\''
      { s }

