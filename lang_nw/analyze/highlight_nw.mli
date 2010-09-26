
val visit_toplevel :
  tag_hook:
    (Ast_nw.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_nw.toplevel * Parser_nw.token list ->
  unit
