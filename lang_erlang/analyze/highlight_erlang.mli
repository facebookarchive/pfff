
val visit_toplevel :
  tag_hook:
    (Ast_erlang.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_erlang.toplevel * Parser_erlang.token list ->
  unit
