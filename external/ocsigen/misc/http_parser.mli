type token =
  | COLON
  | EOL
  | METHOD of (string)
  | PROTO of (string)
  | STRING of (string)
  | CODE of (string)

val header :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ocsigen_http_frame.Http_header.http_header
val nofirstline :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ocsigen_http_frame.Http_header.http_header
