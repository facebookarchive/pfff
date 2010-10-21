
val visit_toplevel :
  tag_hook:
    (Ast_lisp.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_lisp.toplevel * Parser_lisp.token list ->
  unit
