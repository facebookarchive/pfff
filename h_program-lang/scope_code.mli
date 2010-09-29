
type scope = 
  | Global | Local  | Param 
  | Class
  | LocalExn | LocalIterator
  | ListBinded
  | NoScope

val string_of_scope: scope -> string

val vof_scope: scope -> Ocaml.v


val sexp_of_scope: scope -> Sexp.t
val map_scope: scope -> scope
