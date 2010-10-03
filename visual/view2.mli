(*s: view2.mli *)
(*s: mk_gui sig *)
val mk_gui :
  screen_size:int ->
  'b option -> 
  (string (* root *) * 
   Model2.model Model2.async * 
   Model2.drawing * 
   Common.filename option) -> 
  unit
(*e: mk_gui sig *)
(*e: view2.mli *)
