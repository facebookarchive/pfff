
val visit_toplevel :
  tag_hook:(Ast_opa.tok -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Parse_opa.program_and_tokens ->
  unit
