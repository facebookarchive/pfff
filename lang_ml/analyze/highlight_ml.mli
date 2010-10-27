
val visit_toplevel :
  ?lexer_based_tagger:bool ->
  tag_hook:
    (Ast_ml.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Ast_ml.toplevel * Parser_ml.token list ->
  unit
