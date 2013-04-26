

type pattern = Ast_fuzzy.trees

val parse: 
 Common.filename -> pattern

val spatch: 
  pattern -> Common.filename -> string option


