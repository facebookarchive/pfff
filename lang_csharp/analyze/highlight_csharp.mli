
val visit_toplevel :
  tag_hook:
    (Ast_csharp.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_csharp.toplevel * Parser_csharp.token list ->
  unit
