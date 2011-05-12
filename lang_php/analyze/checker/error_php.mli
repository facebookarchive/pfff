
val strict: bool ref

type error = 
  | UndefinedEntity of Entity_php.id_kind * Ast_php.name
  | MultiDefinedEntity of Entity_php.id_kind * Ast_php.name *
      (Ast_php.name * Ast_php.name)

  | TooManyArguments of (Parse_info.info (* call *) * Ast_php.name (* def *))
  | NotEnoughArguments of (Parse_info.info (* call *) * Ast_php.name (* def *))
  | WrongKeywordArgument of  Ast_php.dname * Ast_php.parameter * severity

  | UseOfUndefinedVariable of Ast_php.dname
  | UnusedVariable of Ast_php.dname  * Scope_php.phpscope

  | UseOfUndefinedMember of Ast_php.name
  | UglyGlobalDynamic of Ast_php.info
  | WeirdForeachNoIteratorVar of Ast_php.info

  | CfgError of Controlflow_build_php.error
  | CfgPilError of Controlflow_build_pil.error
 and severity =
   | Bad
   | ReallyBad
   | ReallyReallyBad

val string_of_error: ?show_position_info:bool -> error -> string
val info_of_error: error -> Ast_php.info option
val string_of_severity: severity -> string

exception Error of error

(* ugly global, but sometimes they are practical *)
val _errors: error list ref

val fatal: error -> unit
val warning: error -> unit

val report_error : error -> unit
val report_all_errors: unit -> unit

val rank_errors: error list -> error list
val filter_false_positives: error list -> error list

val show_10_most_recurring_unused_variable_names: unit -> unit

(* Small helper function generating Undefined (or MultiDefined) error 
 * if the entity was not found (or multiply defined). 
 *
 * Note that it memoizes the MultiDefined error so the second time
 * it actually returns the right definition
 *)
val find_entity: 
  find_entity: Entity_php.entity_finder option ->
  (Entity_php.id_kind * Ast_php.name) ->
  Ast_php.entity option
