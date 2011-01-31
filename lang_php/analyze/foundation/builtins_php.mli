(*s: builtins_php.mli *)

val generate_php_stdlib: 
  src:Common.dirname (* dir with .idl.php files *) -> 
  phpmanual_dir:Common.dirname (* dir with php manual xml files *) -> 
  dest:Common.dirname (* e.g. data/php_stdlib/ *) ->
  unit

(* not used anymore *)

type idl_type = 
  | Boolean
  | Byte
  | Int16
  | Int32
  | Int64
  | Double
  | String
  | Int64Vec
  | StringVec
  | VariantVec
  | Int64Map
  | StringMap
  | VariantMap
  | Object
  | Resource
  | Variant
  | Numeric
  | Primitive
  | PlusOperand
  | Sequence
  | Any
  | NULL
  | Void

type idl_param = {
  p_name: string;
  p_type: idl_type;
  p_isref: bool;
  p_default_val: string option;
}

type idl_entry = 
  | Global of string * idl_type
  | Function of 
      string * idl_type * idl_param list * bool (* has variable arguments *)

val actions: unit -> Common.cmdline_actions
(*x: builtins_php.mli *)
(*e: builtins_php.mli *)
