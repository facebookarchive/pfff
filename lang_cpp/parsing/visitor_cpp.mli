
open Ast_cpp

(* the hooks *)
type visitor_in = {
  kexpr: expression vin;
  kfieldkindbis: fieldkindbis vin;
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
