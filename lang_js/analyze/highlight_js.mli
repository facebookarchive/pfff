
val visit_toplevel :
  tag_hook:
    (Ast_js.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_js.toplevel * Parser_js.token list ->
  unit
