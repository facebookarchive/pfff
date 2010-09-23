(* ********************************************************************** *)
(* Writing an Inheritable Class *)
(* ********************************************************************** *)
let pleac_Writing_an_Inheritable_Class () = 
  class person = object (self)
    val mutable name = ""
    val mutable age = 0
    method name = name
    method age = age
    method set_name name' = name <- name'
    method set_age age' = age <- age'
  end
  
  (*-----------------------------*)
  
  let () =
    let dude = new person in
    dude#set_name "Jason";
    dude#set_age 23;
    Printf.printf "%s is age %d\n" dude#name dude#age
  
  (*-----------------------------*)
  
  class employee = object (self)
    inherit person
  end
  
  (*-----------------------------*)
  
  let () =
    let empl = new employee in
    empl#set_name "Jason";
    empl#set_age 23;
    Printf.printf "%s is age %d\n" empl#name empl#age
  

