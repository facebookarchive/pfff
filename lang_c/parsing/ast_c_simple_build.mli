
exception ObsoleteConstruct of string * Parse_info.info
exception CplusplusConstruct
exception TodoConstruct of string * Parse_info.info
exception CaseOutsideSwitch
exception MacroInCase

(* take care! this use Common.gensym to generate fresh unique anon structures
 * so this function may return a different program given the same input
 *)
val program: 
  Ast_cpp.program -> Ast_c.program
