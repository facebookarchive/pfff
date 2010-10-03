(*s: view2.mli *)
(*s: mk_gui sig *)
val mk_gui :
  screen_size:int ->
  (Model2.model Model2.async * Model2.drawing * Common.filename option) -> 
  'b option -> 
  Common.filename list -> 
  unit
(*e: mk_gui sig *)
(*e: view2.mli *)
