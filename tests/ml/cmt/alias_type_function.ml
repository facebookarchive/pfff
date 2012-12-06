
type 'a gen = unit -> 'a

let (ig: int gen) = fun () -> 1

let foo x = ig ()

(*
type expr = Expr
type any = Any

type visitor_in = {
  kexpr: expr vin;
}
  and 'a vin = ('a  -> 'a) * visitor_out -> 'a  -> 'a
and visitor_out = any -> any

let call_continuation = (fun (k, _) x -> k x)

let default_visitor = { 
  kexpr = call_continuation;
}
*)
