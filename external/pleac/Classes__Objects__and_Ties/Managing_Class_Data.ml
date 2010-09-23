(* ********************************************************************** *)
(* Managing Class Data *)
(* ********************************************************************** *)
let pleac_Managing_Class_Data () = 
  (* There are no class methods in OCaml. Use a module instead. *)
  module Person = struct
    let _body_count = ref 0
    let population () = !_body_count
    let destroy person = decr _body_count
    class person = object (self)
      initializer
        incr _body_count;
        Gc.finalise destroy self
    end
  end
  
  (* Later, the user can say this: *)
  let () =
    let people = ref [] in
    for i = 1 to 10 do people := new Person.person :: !people done;
    Printf.printf "There are %d people alive.\n" (Person.population ())
    (* There are 10 people alive. *)
  
  (* A class with an attribute that changes all instances when set. *)
  module FixedArray = struct
    let _bounds = ref 7  (* default *)
    let max_bounds () = !_bounds
    let set_max_bounds max = _bounds := max
    class fixed_array = object
      method max_bounds = !_bounds
      method set_max_bounds bounds' = _bounds := bounds'
    end
  end
  let () =
    (* Set for whole class *)
    FixedArray.set_max_bounds 100;
    let alpha = new FixedArray.fixed_array in
    Printf.printf "Bound on alpha is %d\n" alpha#max_bounds;
    (* 100 *)
    let beta = new FixedArray.fixed_array in
    beta#set_max_bounds 50;
    Printf.printf "Bound on alpha is %d\n" alpha#max_bounds;
    (* 50 *)
  
  (* To make the bounds read only, just remove the set method. *)
  

