
val visit_toplevel :
  tag_hook:
    (Ast_hs.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_hs.toplevel * Parser_hs.token list ->
  unit
