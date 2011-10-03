
open Ast_mini_php

type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (stmt  -> unit) * visitor_out -> stmt  -> unit;
  ktop: (toplevel -> unit) * visitor_out -> toplevel  -> unit;
}

and visitor_out = {
  vexpr: expr  -> unit;
  vstmt: stmt -> unit;
  vtop: toplevel -> unit;
}

val default_visitor: visitor_in
val mk_visitor: visitor_in -> visitor_out

