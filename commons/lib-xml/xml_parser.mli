type token =
  | NEXT
  | OR
  | IDENT of (string)
  | PCDATA
  | STAR
  | QUESTION
  | PLUS
  | OPEN
  | CLOSE
  | END

val dtd_element :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Dtd_types.dtd_child
