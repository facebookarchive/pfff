(*s: dataflow_php.mli *)

type 'a env = 
  (Ast_php.dname, 'a) Common.assoc

type 'a inout = {
  in_env: 'a env;
  out_env: 'a env;
}

type 'a mapping = 
  (Ograph_extended.nodei, 'a inout) Common.assoc


val fixpoint: 
  Controlflow_php.flow -> 
  initial:('a mapping) -> 
  transfer:('a mapping (*in*) -> Ograph_extended.nodei ->'a mapping (*out*)) ->
  join: ('a mapping list -> 'a mapping) ->
  'a mapping



val display_dflow: 
  Controlflow_php.flow -> 'a mapping -> ('a -> string) -> unit

(*x: dataflow_php.mli *)
(*e: dataflow_php.mli *)
