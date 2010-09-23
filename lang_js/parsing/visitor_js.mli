
open Ast_js

(* the hooks *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (st  -> unit) * visitor_out -> st  -> unit;
  kfield: (field -> unit) * visitor_out -> field -> unit;
  kinfo: (info -> unit)  * visitor_out -> info  -> unit;
}
and visitor_out = {
  vexpr: expr  -> unit;
  vst: st -> unit;
  vtop: toplevel -> unit;
  vinfo: info -> unit;
  vprogram: program -> unit;
}


val default_visitor : visitor_in

val mk_visitor: visitor_in -> visitor_out
