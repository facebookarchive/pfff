(*s: view2.mli *)
(*s: mk_gui sig *)
val mk_gui :
  screen_size:int ->
  legend:bool ->
  'b option -> 
  (string (* root *) * Model2.world) -> 
  unit
(*e: mk_gui sig *)
(*e: view2.mli *)
