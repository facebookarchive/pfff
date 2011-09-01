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
  klet_binding: let_binding vin;
  kqualifier: qualifier vin;
  kmodule_expr: module_expr vin;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and visitor_out = any -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
