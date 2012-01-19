
val visit_toplevel :
  tag_hook:(Ast_opa.tok -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_opa.toplevel * Parser_opa.token list ->
  unit
