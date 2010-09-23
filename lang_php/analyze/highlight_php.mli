
val visit_toplevel :
  tag:
    (Ast_php.info -> Highlight_code.category -> unit) ->
  maybe_add_has_type_icon:
    (Database_php.id -> Ast_php.info -> Database_php.database -> unit) ->
  Highlight_code.highlighter_preferences ->
  (Database_php.id * Common.filename * Database_php.database) option -> 
  Ast_php.toplevel * Parser_php.token list ->
  unit
