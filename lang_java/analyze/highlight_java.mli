
val visit_toplevel :
  tag_hook:
    (Ast_java.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_java.toplevel * Parser_java.token list ->
  unit
