(* ********************************************************************** *)
(* Cloning Objects *)
(* ********************************************************************** *)
let pleac_Cloning_Objects () = 
  (* Objects can be cloned with Oo.copy. *)
  let ob1 = new some_class
  (* later on *)
  let ob2 = Oo.copy ob1
  
  (* Objects can also be cloned using the functional update syntax. *)
  class person (name : string) (age : int) = object
    val name = name
    val age = age
    method name = name
    method age = age
    method with_name name' = {< name = name' >}
    method with_age age' = {< age = age' >}
    method copy () = {< >}
  end
  

