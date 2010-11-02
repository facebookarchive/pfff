
type visitor_in = {
  kvar: Pil.var vin;
  klvalue: Pil.lvalue vin;
  kexpr: Pil.expr vin;
  kinstr: Pil.instr vin;
  kstmt: Pil.stmt vin;
}
and visitor_out = {
  vstmt: Pil.stmt vout;
  vstmt_list: (Pil.stmt list) vout;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and 'a vout = 'a -> unit

val default_visitor: visitor_in 

val mk_visitor: visitor_in -> visitor_out

val do_visit_with_ref:
  ('a list ref -> visitor_in) -> 
  (visitor_out -> unit) -> 'a list

val do_visit_with_h:
  (('a, 'b) Hashtbl.t -> visitor_in) -> 
  (visitor_out -> unit) -> ('a, 'b) Hashtbl.t
