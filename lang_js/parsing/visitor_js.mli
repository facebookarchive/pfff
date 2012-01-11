
open Ast_js

(* the hooks *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (st  -> unit) * visitor_out -> st  -> unit;
  kfield: (field -> unit) * visitor_out -> field -> unit;
  kinfo: (tok -> unit)  * visitor_out -> tok  -> unit;
}
and visitor_out = any -> unit

val default_visitor : visitor_in

val mk_visitor: visitor_in -> visitor_out

(* poor's man fold *)
val do_visit_with_ref:
  ('a list ref -> visitor_in) -> any -> 'a list
