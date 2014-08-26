
val interfaces:
  Ast_php.class_def -> Ast_php.class_name list
val traits:
  Ast_php.class_def -> Ast_php.class_name list

(* __construct *)
val constructor_name: string 
val get_constructor: Ast_php.class_def -> Ast_php.method_def

val get_public_or_protected_vars_of_class:
  Ast_php.class_def -> Ast_php.dname list

val is_static_method:
  Ast_php.method_def -> bool
val has_visiblity_modifier:
  Ast_php.modifier Ast_php.wrap list -> bool
val is_interface:
  Ast_php.class_def -> bool

exception Use__Call
exception UndefinedClassWhileLookup of string

(* can raise UndefinedClassWhileLookup, Not_found, Multi_found, or Use__Call *)
val lookup_method: 
  ?case_insensitive: bool ->
  (string (* class *) * string (* method *)) ->
  Entity_php.entity_finder -> Ast_php.method_def

(* can raise UndefinedClassWhileLookup, Not_found, Multi_found *)
val lookup_member: 
  ?case_insensitive: bool ->
  (string (* class *) * string (* field *)) ->
  Entity_php.entity_finder ->
  Ast_php.class_variable * Ast_php.class_var_modifier

(* can raise UndefinedClassWhileLookup, Not_found, Multi_found *)
val lookup_constant:
  (string (* class *) * string (* constant *)) ->
  Entity_php.entity_finder ->
  Ast_php.class_constant

(* does not raise exception *)
val collect_members: 
  string (* class *) -> Entity_php.entity_finder -> Ast_php.dname list



val class_variables_reorder_first:
  Ast_php.class_def -> Ast_php.class_def

val class_kind_of_ctype:
  Ast_php.class_type -> Entity_code.class_kind
val string_of_class_type:
  Ast_php.class_type -> string

