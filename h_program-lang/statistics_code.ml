open Common

(* was first done for CComment *)
(*****************************************************************************)
(* Entities stat *)
(*****************************************************************************)
(* 
 * This module can be used to store stat on code entities or commented 
 * code entities, be it C, C++, or Java entities. 
 * 
 * The numbers can represent anything.
 * 
 * todo?: a little bit of overlap with Comments.place ? and quite tedious
 * all those fields to manipulate. OCaml not very good with fields to
 * factorize code. So change ? use a hash ? use Comments.place ? 
 * But Comments.place type quite suited to the fact that all comments
 * does not have multiple place but are a choice.
 * 
 * note: no need for mutable nb_header: int; :) 
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

(* constructor :) *)
let default_entities_stat () = {
  nb_define_var = 0;
  nb_define_func = 0;
  nb_include = 0;

  nb_function = 0;
  nb_parameter = 0;
  nb_prototype = 0;
  nb_variable = 0;
  nb_struct = 0;
  nb_field = 0;
  nb_enum = 0;
  nb_enum_value = 0;

  nb_class = 0;
  nb_method = 0;

  nb_loop = 0;
  nb_if = 0;
  nb_else = 0;
  nb_goto = 0;
  nb_continue = 0;
  nb_break = 0;
  nb_return = 0;
  nb_label = 0;
  nb_case = 0;

  nb_try = 0;

  nb_simple_funcall = 0;
  nb_method_call = 0;
  nb_simple_assign = 0;
  nb_simple_field_assign = 0;
  nb_initializer = 0;
}

let op_entities_stat (+) st1 st2 = {
  nb_define_var = st1.nb_define_var + st2.nb_define_var;
  nb_define_func = st1.nb_define_func + st2.nb_define_func;
  nb_include = st1.nb_include + st2.nb_include;

  nb_function = st1.nb_function + st2.nb_function;
  nb_parameter = st1.nb_parameter + st2.nb_parameter;
  nb_prototype = st1.nb_prototype + st2.nb_prototype;
  nb_variable = st1.nb_variable + st2.nb_variable;
  nb_struct = st1.nb_struct + st2.nb_struct;
  nb_field = st1.nb_field + st2.nb_field;
  nb_enum = st1.nb_enum + st2.nb_enum;
  nb_enum_value = st1.nb_enum_value + st2.nb_enum_value;

  nb_class = st1.nb_class + st2.nb_class;
  nb_method = st1.nb_method + st2.nb_method;

  nb_loop = st1.nb_loop + st2.nb_loop;
  nb_if = st1.nb_if + st2.nb_if;
  nb_else = st1.nb_else + st2.nb_else;
  nb_goto = st1.nb_goto + st2.nb_goto;
  nb_continue = st1.nb_continue + st2.nb_continue;
  nb_break = st1.nb_break + st2.nb_break;
  nb_return = st1.nb_return + st2.nb_return;
  nb_label = st1.nb_label + st2.nb_label;
  nb_case = st1.nb_case + st2.nb_case;

  nb_try = st1.nb_try + st2.nb_try;

  nb_simple_funcall = st1.nb_simple_funcall + st2.nb_simple_funcall;
  nb_method_call = st1.nb_method_call + st2.nb_method_call;
  nb_simple_assign = st1.nb_simple_assign + st2.nb_simple_assign;
  nb_simple_field_assign = st1.nb_simple_field_assign + st2.nb_simple_field_assign;
  nb_initializer = st1.nb_initializer + st2.nb_initializer;
}

let add_entities_stat st1 st2 = 
  op_entities_stat (+) st1 st2



let sum_entities_stat_list xs = 
  xs +> List.fold_left add_entities_stat (default_entities_stat())

let div_pourcent_entities_stat st1 st2 = 
  let (+++) n1 n2 = 
    if n2 = 0 
    then -1 
    else (n1 * 100) / n2
  in
  op_entities_stat (+++) st1 st2
      

let print_entities_stat_list = fun xs -> 
  let _files_total = (List.length xs) in
  let sum = sum_entities_stat_list xs in
  begin
    pr2 (spf "NB define_var = %d" sum.nb_define_var);
    pr2 (spf "NB define_func = %d" sum.nb_define_func);
    pr2 (spf "NB include = %d" sum.nb_include);

    pr2 (spf "NB function = %d" sum.nb_function);
    pr2 (spf "NB parameter = %d" sum.nb_parameter);
    pr2 (spf "NB prototype = %d" sum.nb_prototype);
    pr2 (spf "NB variable = %d" sum.nb_variable);
    pr2 (spf "NB struct = %d" sum.nb_struct);
    pr2 (spf "NB field = %d" sum.nb_field);
    pr2 (spf "NB enum = %d" sum.nb_enum);
    pr2 (spf "NB enum_value = %d" sum.nb_enum_value);

    pr2 (spf "NB class = %d" sum.nb_class);
    pr2 (spf "NB method = %d" sum.nb_method);

    pr2 (spf "NB loop = %d" sum.nb_loop);
    pr2 (spf "NB if = %d" sum.nb_if);
    pr2 (spf "NB else = %d" sum.nb_else);
    pr2 (spf "NB goto = %d" sum.nb_goto);
    pr2 (spf "NB continue = %d" sum.nb_continue);
    pr2 (spf "NB break = %d" sum.nb_break);
    pr2 (spf "NB return = %d" sum.nb_return);
    pr2 (spf "NB label = %d" sum.nb_label);
    pr2 (spf "NB case = %d" sum.nb_case);

    pr2 (spf "NB try = %d" sum.nb_try);

    pr2 (spf "NB simple_funcall = %d" sum.nb_simple_funcall);
    pr2 (spf "NB method_call = %d" sum.nb_method_call);
    pr2 (spf "NB simple_assign = %d" sum.nb_simple_assign);
    pr2 (spf "NB simple_field_assign = %d" sum.nb_simple_field_assign);
    pr2 (spf "NB initializer = %d" sum.nb_initializer);
  end

let hash_of_entities_stat sum = 
  let h = Hashtbl.create 101 in 
  begin
    Hashtbl.add h "define_var" sum.nb_define_var;
    Hashtbl.add h "define_func" sum.nb_define_func;
    Hashtbl.add h "include" sum.nb_include;

    Hashtbl.add h "function" sum.nb_function;
    Hashtbl.add h "parameter" sum.nb_parameter;
    Hashtbl.add h "prototype" sum.nb_prototype;
    Hashtbl.add h "variable" sum.nb_variable;
    Hashtbl.add h "struct" sum.nb_struct;
    Hashtbl.add h "field" sum.nb_field;
    Hashtbl.add h "enum" sum.nb_enum;
    Hashtbl.add h "enum_value" sum.nb_enum_value;

    Hashtbl.add h "class" sum.nb_class;
    Hashtbl.add h "method" sum.nb_method;

    Hashtbl.add h "loop" sum.nb_loop;
    Hashtbl.add h "if" sum.nb_if;
    Hashtbl.add h "else" sum.nb_else;
    Hashtbl.add h "goto" sum.nb_goto;
    Hashtbl.add h "continue" sum.nb_continue;
    Hashtbl.add h "break" sum.nb_break;
    Hashtbl.add h "return" sum.nb_return;
    Hashtbl.add h "label" sum.nb_label;
    Hashtbl.add h "case" sum.nb_case;

    Hashtbl.add h "try" sum.nb_try;

    Hashtbl.add h "simple_funcall" sum.nb_simple_funcall;
    Hashtbl.add h "method_call" sum.nb_method_call;
    Hashtbl.add h "simple_assign" sum.nb_simple_assign;
    Hashtbl.add h "simple_field_assign" sum.nb_simple_field_assign;
    Hashtbl.add h "initializer" sum.nb_initializer;

    h
  end

let assoc_of_entities_stat x = 
  x +> hash_of_entities_stat +> Common.hash_to_list


