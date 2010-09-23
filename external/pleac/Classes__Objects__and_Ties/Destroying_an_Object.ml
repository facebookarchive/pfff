(* ********************************************************************** *)
(* Destroying an Object *)
(* ********************************************************************** *)
let pleac_Destroying_an_Object () = 
  (* The Gc.finalise function can be used to create finalizers,
     which are like destructors but run at garbage collection time,
     for any value, not just objects. You can still use a method if
     you want: *)
  
  class klass =
  object (self)
    initializer
      Gc.finalise (fun self -> self#destroy ()) self
    method destroy () =
      Printf.printf "klass %d is dying\n" (Oo.id self)
  end
  let () =
    ignore (new klass);
    Gc.full_major ()
  
  (* The "destroy" method above is public. If you want to keep it
     hidden, you can create a finalizer in a let-binding instead: *)
  
  class klass =
    let destroy obj =
      Printf.printf "klass %d is dying\n" (Oo.id obj) in
  object (self)
    initializer Gc.finalise destroy self
  end
  

