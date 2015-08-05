
val visit_program :
  tag_hook:
    (Parse_info.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_hs.program * Parser_hs.token list ->
  unit
