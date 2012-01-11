type visitor_in = {
  kexpr : Ast_js.expr vin;
  kst : Ast_js.st vin;
  ktoplevel : Ast_js.toplevel vin;
  kany : Ast_js.any vin;
  kinfo : Ast_js.tok vin;
  kname : Ast_js.name vin;
}
  and 'a vin = ('a -> 'a) * visitor_out -> 'a -> 'a
and visitor_out = Ast_js.any -> Ast_js.any

val default_visitor : visitor_in

val mk_visitor : visitor_in -> visitor_out
