

(* entities_stat is independent of comments so we put it in
 * another file but as the visitor for comments extraction already talk
 * about all the entities we want stat about, it's easier to do
 * everything in the same visitor, so the entities_stat computation
 * are still actually in comments_extraction.ml.
 *)
type entities_stat = { 
  mutable nb_define_var: int;
  mutable nb_define_func: int;
  mutable nb_include: int;

  mutable nb_function: int;
  mutable nb_parameter: int;
  mutable nb_prototype: int;
  mutable nb_variable: int;
  mutable nb_struct: int;
  mutable nb_field: int;
  mutable nb_enum: int;
  mutable nb_enum_value: int;

  mutable nb_class: int;
  mutable nb_method: int;

  mutable nb_loop: int;
  mutable nb_if: int;
  mutable nb_else: int;
  mutable nb_goto: int;
  mutable nb_continue: int;
  mutable nb_break: int;
  mutable nb_return: int;
  mutable nb_label: int;
  mutable nb_case: int;

  mutable nb_try: int;

  mutable nb_simple_funcall: int;
  mutable nb_method_call: int;
  mutable nb_simple_assign: int;
  mutable nb_simple_field_assign: int;
  mutable nb_initializer: int;
  
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


