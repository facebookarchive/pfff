

type pattern = Ast_fuzzy.trees

val parse: 
 pattern_of_string:(string -> pattern) ->
 ii_of_pattern:(pattern -> Parse_info.info list) ->
 Common.filename -> pattern

val spatch: 
  pattern -> Common.filename -> string option


