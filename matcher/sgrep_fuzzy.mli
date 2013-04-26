
type pattern = Ast_fuzzy.trees

val sgrep:   
  hook:(Metavars_fuzzy.fuzzy_binding -> Parse_info.info list -> unit) ->
  pattern -> Ast_fuzzy.trees -> unit
