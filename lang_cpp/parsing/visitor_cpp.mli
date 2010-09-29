
open Ast_cpp

(* the hooks *)
type visitor_in = {
  kexpr: expression vin;
  kfieldkind: fieldkind vin;
  kparameterType: parameterType vin;
  ktypeC: typeC vin;
  kvar_declaration: var_declaration vin;
}
and visitor_out = {
  vexpr: expression vout;
  vprogram: program vout;
  vtoplevel: toplevel vout;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and 'a vout = 'a -> unit

val default_visitor : visitor_in

val mk_visitor: visitor_in -> visitor_out
