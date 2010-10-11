
val visit_toplevel :
  tag: (Ast_php.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (string, Database_code.entity) Hashtbl.t -> 
  Ast_php.toplevel * Parser_php.token list ->
  unit
