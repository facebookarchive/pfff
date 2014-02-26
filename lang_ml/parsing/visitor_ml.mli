
type visitor_in = {
  kitem: Ast_ml.item vin;
  ktoplevel: Ast_ml.toplevel vin;
  kexpr: Ast_ml.expr vin;
  kpattern: Ast_ml.pattern vin;
  kty: Ast_ml.ty vin;
  kfield_decl: Ast_ml.field_declaration vin;
  kfield_expr: Ast_ml.field_and_expr vin;
  kfield_pat: Ast_ml.field_pattern vin;
  ktype_declaration: Ast_ml.type_declaration vin;
  klet_def: Ast_ml.let_def vin;
  klet_binding: Ast_ml.let_binding vin;
  kqualifier: Ast_ml.qualifier vin;
  kmodule_expr: Ast_ml.module_expr vin;
  kparameter: Ast_ml.parameter vin;
  kargument: Ast_ml.argument vin;
  kinfo: Ast_ml.tok vin;
}
and visitor_out = Ast_ml.any -> unit
  and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
