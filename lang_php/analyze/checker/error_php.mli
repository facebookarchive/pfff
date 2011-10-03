
val strict: bool ref

type error = {
  typ: error_kind;
  loc: Ast_php.info;
  sev: severity;
}
 and severity = Fatal | Warning

 and error_kind = 
  | UndefinedEntity    of Entity_php.id_kind * string (* name *)
  | MultiDefinedEntity of Entity_php.id_kind * string (* name *) *
      (string * string) (* name * name *)

  | TooManyArguments   of string (* name *) (* def *)
  | NotEnoughArguments of string (* name *) (* def *)
  | WrongKeywordArgument of
      string (* dname *) * string (* parameter *) * severity2
        
  | UseOfUndefinedVariable of string (* dname *)
  | UnusedVariable of string (* dname *)  * Scope_php.phpscope
  | UseOfUndefinedVariableInLambda of string (* dname *)

  | UseOfUndefinedMember of string (* name *)
  | UglyGlobalDynamic
  | WeirdForeachNoIteratorVar

  | CfgError of Controlflow_build_php.error_kind
(*  | CfgPilError of Controlflow_build_pil.error_kind *)

  | FileNotFound of Common.filename

  and severity2 =
    | Bad
    | ReallyBad
    | ReallyReallyBad

val string_of_error: error -> string
val string_of_error_kind: error_kind -> string

val string_of_severity2: severity2 -> string

exception Error of error

(* ugly global, but sometimes they are practical *)
val _errors: error list ref

val fatal: Ast_php.info -> error_kind -> unit
val warning: Ast_php.info -> error_kind -> unit

val report_error : error -> unit
val report_all_errors: unit -> unit

val rank_errors: error list -> error list

val show_10_most_recurring_unused_variable_names: unit -> unit

(* Small helper function generating Undefined (or MultiDefined) error 
 * if the entity was not found (or multiply defined). 
 *
 * Note that it memoizes the MultiDefined error so the second time
 * it actually returns the right definition
 *)
val find_entity_and_warn: 
  find_entity: Entity_php.entity_finder option ->
  (Entity_php.id_kind * Ast_php.name) ->
  Ast_php.entity option
