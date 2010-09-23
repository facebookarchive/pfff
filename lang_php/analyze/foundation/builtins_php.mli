(*s: builtins_php.mli *)

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


val ast_php_to_idl: 
  Ast_php.program -> idl_entry list

val idl_entry_to_php_fake_code: 
  idl_entry -> string


val generate_php_stdlib: 
  Common.dirname (* dir with .idl.php files *) -> 
  Common.dirname (* dir with php manual xml files *) -> 
  Common.dirname (* dest, e.g. data/php_stdlib/ *) ->
  unit

val actions: unit -> Common.cmdline_actions
(*x: builtins_php.mli *)
(*e: builtins_php.mli *)
