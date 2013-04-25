
type pattern = Ast_fuzzy.tree list

(* val parse: string -> pattern *)

val sgrep:   
  hook:('a Metavars_fuzzy.metavars_binding -> Parse_info.info list -> unit) ->
  pattern -> Ast_fuzzy.tree list -> unit
