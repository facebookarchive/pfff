(*s: visitor2_php.mli *)

open Ast_php

type visitor_out = {
  vorigin: Visitor_php.visitor_out;
  vid_ast: Ast_entity_php.id_ast -> unit;
}

val default_visitor : Visitor_php.visitor_in

val mk_visitor: Visitor_php.visitor_in -> visitor_out

val do_visit_with_ref:
  ('a list ref -> Visitor_php.visitor_in) -> (visitor_out -> unit) -> 'a list
(*x: visitor2_php.mli *)
(*e: visitor2_php.mli *)
