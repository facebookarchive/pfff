open Tata

type toto = {x: int; y: int    ;}

val main (* putain de commentaire *) :
  ((((Lexing.lexbuf  -> token)))) -> Lexing.lexbuf -> AbstractSyntax.signature
val signature :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbstractSyntax.signature

(* end commentary *)
