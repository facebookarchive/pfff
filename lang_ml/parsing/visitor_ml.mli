open Ast_ml

type visitor_in = {
  kinfo: info vin;
  kexpr: expr vin;
  kfield_decl: field_declaration vin;
  kfield_expr: field_and_expr vin;
  kty: ty vin;
  ktype_declaration: type_declaration vin;
  kpattern: pattern vin;
  kitem: item vin;
  klet_def: let_def vin;
}
and visitor_out = {
  vtoplevel: toplevel vout;
  vprogram: program vout;
  vmatch_case: match_case vout;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and 'a vout = 'a -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
