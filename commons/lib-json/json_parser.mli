type token =
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | OBJSTART
  | OBJEND
  | ARSTART
  | AREND
  | NULL
  | COMMA
  | COLON
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Json_type.t
