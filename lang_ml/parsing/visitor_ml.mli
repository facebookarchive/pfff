open Ast_ml

type visitor_in = {
  kinfo: (info -> unit)  * visitor_out -> info  -> unit;
}
and visitor_out = {
  vtoplevel: toplevel -> unit;
  vprogram: program -> unit;
}

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
