(* ********************************************************************** *)
(* Solving the Data Inheritance Problem *)
(* ********************************************************************** *)
let pleac_Solving_the_Data_Inheritance_Problem () = 
  (* Use prefixes in instance variable names so we can tell them apart. *)
  class person () = object
    val mutable person_age = 0
    method age = person_age
    method set_age age' = person_age <- age'
  end
  
  (* Now we can access both instance variables as needed. *)
  class employee () = object
    inherit person ()
    val mutable employee_age = 0
    method age = employee_age
    method set_age age' = employee_age <- age'
    method person_age = person_age
    method set_person_age age' = person_age <- age'
  end
  

