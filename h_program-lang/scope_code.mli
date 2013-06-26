
type scope = 
  | Global | Local  | Param | Static | Class

  | LocalExn | LocalIterator
  | ListBinded
  | Closed

  | NoScope

val string_of_scope: scope -> string
val vof_scope: scope -> Ocaml.v
