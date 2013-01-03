open Common

(*****************************************************************************)
(* Entities stat *)
(*****************************************************************************)
(* 
 * This module can be used to store stat on code entities or commented 
 * code entities. 
 * 
 * The numbers can represent anything.
 * 
 * history: 
 *  - was first written for CComment and the comment study
 * 
 * note: no need for mutable nb_header: int; :) 
 * 
 * todo?: a little bit of overlap with Comments.place ? and quite tedious
 * all those fields to manipulate. OCaml not very good with fields to
 * factorize code. So change ? use a hash ? use Comments.place ? 
 * But Comments.place type quite suited to the fact that all comments
 * does not have multiple place but are a choice.
 * 
 *)
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

(* constructor :) *)
let default_entities_stat () = {
  define_var = 0;
  define_func = 0;
  include_ = 0;

  function_ = 0;
  parameter = 0;
  prototype = 0;
  variable = 0;
  struct_ = 0;
  field = 0;
  enum = 0;
  enum_value = 0;

  class_ = 0;
  method_ = 0;

  loop = 0;
  if_ = 0;
  else_ = 0;
  goto = 0;
  continue = 0;
  break = 0;
  return = 0;
  label = 0;
  case = 0;

  try_ = 0;

  simple_funcall = 0;
  method_call = 0;
  simple_assign = 0;
  simple_field_assign = 0;
  initializer_ = 0;
}

let op_entities_stat (+) st1 st2 = {
  define_var = st1.define_var + st2.define_var;
  define_func = st1.define_func + st2.define_func;
  include_ = st1.include_ + st2.include_;

  function_ = st1.function_ + st2.function_;
  parameter = st1.parameter + st2.parameter;
  prototype = st1.prototype + st2.prototype;
  variable = st1.variable + st2.variable;
  struct_ = st1.struct_ + st2.struct_;
  field = st1.field + st2.field;
  enum = st1.enum + st2.enum;
  enum_value = st1.enum_value + st2.enum_value;

  class_ = st1.class_ + st2.class_;
  method_ = st1.method_ + st2.method_;

  loop = st1.loop + st2.loop;
  if_ = st1.if_ + st2.if_;
  else_ = st1.else_ + st2.else_;
  goto = st1.goto + st2.goto;
  continue = st1.continue + st2.continue;
  break = st1.break + st2.break;
  return = st1.return + st2.return;
  label = st1.label + st2.label;
  case = st1.case + st2.case;

  try_ = st1.try_ + st2.try_;

  simple_funcall = st1.simple_funcall + st2.simple_funcall;
  method_call = st1.method_call + st2.method_call;
  simple_assign = st1.simple_assign + st2.simple_assign;
  simple_field_assign = st1.simple_field_assign + st2.simple_field_assign;
  initializer_ = st1.initializer_ + st2.initializer_;
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
    pr2 (spf "NB define_var = %d" sum.define_var);
    pr2 (spf "NB define_func = %d" sum.define_func);
    pr2 (spf "NB include = %d" sum.include_);

    pr2 (spf "NB function = %d" sum.function_);
    pr2 (spf "NB parameter = %d" sum.parameter);
    pr2 (spf "NB prototype = %d" sum.prototype);
    pr2 (spf "NB variable = %d" sum.variable);
    pr2 (spf "NB struct = %d" sum.struct_);
    pr2 (spf "NB field = %d" sum.field);
    pr2 (spf "NB enum = %d" sum.enum);
    pr2 (spf "NB enum_value = %d" sum.enum_value);

    pr2 (spf "NB class = %d" sum.class_);
    pr2 (spf "NB method = %d" sum.method_);

    pr2 (spf "NB loop = %d" sum.loop);
    pr2 (spf "NB if = %d" sum.if_);
    pr2 (spf "NB else = %d" sum.else_);
    pr2 (spf "NB goto = %d" sum.goto);
    pr2 (spf "NB continue = %d" sum.continue);
    pr2 (spf "NB break = %d" sum.break);
    pr2 (spf "NB return = %d" sum.return);
    pr2 (spf "NB label = %d" sum.label);
    pr2 (spf "NB case = %d" sum.case);

    pr2 (spf "NB try = %d" sum.try_);

    pr2 (spf "NB simple_funcall = %d" sum.simple_funcall);
    pr2 (spf "NB method_call = %d" sum.method_call);
    pr2 (spf "NB simple_assign = %d" sum.simple_assign);
    pr2 (spf "NB simple_field_assign = %d" sum.simple_field_assign);
    pr2 (spf "NB initializer = %d" sum.initializer_);
  end

let hash_of_entities_stat sum = 
  let h = Hashtbl.create 101 in 
  begin
    Hashtbl.add h "define_var" sum.define_var;
    Hashtbl.add h "define_func" sum.define_func;
    Hashtbl.add h "include" sum.include_;

    Hashtbl.add h "function" sum.function_;
    Hashtbl.add h "parameter" sum.parameter;
    Hashtbl.add h "prototype" sum.prototype;
    Hashtbl.add h "variable" sum.variable;
    Hashtbl.add h "struct" sum.struct_;
    Hashtbl.add h "field" sum.field;
    Hashtbl.add h "enum" sum.enum;
    Hashtbl.add h "enum_value" sum.enum_value;

    Hashtbl.add h "class" sum.class_;
    Hashtbl.add h "method" sum.method_;

    Hashtbl.add h "loop" sum.loop;
    Hashtbl.add h "if" sum.if_;
    Hashtbl.add h "else" sum.else_;
    Hashtbl.add h "goto" sum.goto;
    Hashtbl.add h "continue" sum.continue;
    Hashtbl.add h "break" sum.break;
    Hashtbl.add h "return" sum.return;
    Hashtbl.add h "label" sum.label;
    Hashtbl.add h "case" sum.case;

    Hashtbl.add h "try" sum.try_;

    Hashtbl.add h "simple_funcall" sum.simple_funcall;
    Hashtbl.add h "method_call" sum.method_call;
    Hashtbl.add h "simple_assign" sum.simple_assign;
    Hashtbl.add h "simple_field_assign" sum.simple_field_assign;
    Hashtbl.add h "initializer" sum.initializer_;

    h
  end

let assoc_of_entities_stat x = 
  x +> hash_of_entities_stat +> Common.hash_to_list


