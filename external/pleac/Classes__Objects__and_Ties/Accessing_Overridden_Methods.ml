(* ********************************************************************** *)
(* Accessing Overridden Methods *)
(* ********************************************************************** *)
let pleac_Accessing_Overridden_Methods () = 
  class person (name : string) (age : int) = object
    val mutable name = name
    val mutable age = age
    method name = name
    method age = age
    method set_name name' = name <- name'
    method set_age age' = age <- age'
  end
  
  class liar name age = object
    (* Call superclass constructor and alias superclass as "super". *)
    inherit person name age as super
    (* Call overridden "age" method. *)
    method age = super#age - 10
  end
  

