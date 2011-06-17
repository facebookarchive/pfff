
open Ast_cpp

(* the hooks *)
type visitor_in = {
  kexpr: expression vin;
  kstmt: statement vin;

  kfieldkind: fieldkind vin;
  kparameterType: parameterType vin;
  ktypeC: typeC vin;
  kvar_declaration: var_declaration vin;
  kcompound: compound vin;
}
and visitor_out = any -> unit
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

val default_visitor : visitor_in

val mk_visitor: visitor_in -> visitor_out
