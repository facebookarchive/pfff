
type entities_stat = { 
  mutable define_var: int;
  mutable define_func: int;
  mutable include_: int;

  mutable function_: int;
  mutable parameter: int;
  mutable prototype: int;
  mutable variable: int;
  mutable struct_: int;
  mutable field: int;
  mutable enum: int;
  mutable enum_value: int;

  mutable class_: int;
  mutable method_: int;

  mutable loop: int;
  mutable if_: int;
  mutable else_: int;
  mutable goto: int;
  mutable continue: int;
  mutable break: int;
  mutable return: int;
  mutable label: int;
  mutable case: int;

  mutable try_: int;

  mutable simple_funcall: int;
  mutable method_call: int;
  mutable simple_assign: int;
  mutable simple_field_assign: int;
  mutable initializer_: int;
  
}
val default_entities_stat : unit -> entities_stat
val print_entities_stat_list : entities_stat list -> unit

val add_entities_stat : 
  entities_stat -> entities_stat -> entities_stat
val div_pourcent_entities_stat : 
  entities_stat -> entities_stat -> entities_stat

val sum_entities_stat_list : entities_stat list -> entities_stat

val hash_of_entities_stat : entities_stat -> (string, int) Hashtbl.t
val assoc_of_entities_stat : entities_stat -> (string, int) Common.assoc


