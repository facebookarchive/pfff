open Ast_ml

type visitor_in = {
  kinfo: info vin;
  kexpr: expr vin;
}
and visitor_out = {
  vtoplevel: toplevel vout;
  vprogram: program vout;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and 'a vout = 'a -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
