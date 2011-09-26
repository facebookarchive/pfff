
type visitor_in = {
  kvar: Pil.var vin;

  klvalue: Pil.lvalue vin;
  kexpr: Pil.expr vin;
  kinstr: Pil.instr vin;
  kstmt: Pil.stmt vin;

}
  and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and visitor_out = Controlflow_pil.any -> unit

val default_visitor: visitor_in 
val mk_visitor: visitor_in -> visitor_out

val do_visit_with_ref:
  ('a list ref -> visitor_in) -> Controlflow_pil.any -> 
  'a list

val do_visit_with_h:
  (('a, 'b) Hashtbl.t -> visitor_in) -> Controlflow_pil.any -> 
  ('a, 'b) Hashtbl.t
