
(* does side effects on Error_php._errors *)
val check_program: 
  ?find_entity: Entity_php.entity_finder option ->
  Ast_php.program -> unit
