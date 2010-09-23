(*s: scope_php.mli *)
(*s: type phpscope *)
type phpscope = 
  | Global
  | Local 
  | Param

  | Class

  | LocalExn
  | LocalIterator
  | ListBinded
  (* | Class ? *)

  | NoScope
 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)
(*e: type phpscope *)

val s_of_phpscope: phpscope -> string
(*e: scope_php.mli *)
