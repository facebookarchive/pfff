(* ********************************************************************** *)
(* Determining Subclass Membership *)
(* ********************************************************************** *)
let pleac_Determining_Subclass_Membership () = 
  (* OCaml has no runtime type information and therefore no "instanceof"
     operator. One alternative would be to provide methods to query for
     an object's class. *)
  class widget (name : string) = object
    method name = name
    method is_widget = true
    method is_gadget = false
  end
  class gadget name = object
    inherit widget name
    method is_gadget = true
  end
  
  (* Another solution would be to use the visitor pattern. *)
  class widget (name : string) = object (self)
    method name = name
    method accept (v : visitor) = v#visit_widget (self :> widget)
  end
  and gadget name = object (self)
    inherit widget name
    method accept (v : visitor) = v#visit_gadget (self :> gadget)
  end
  and visitor ~visit_widget ~visit_gadget = object
    method visit_widget = (visit_widget : widget -> unit)
    method visit_gadget = (visit_gadget : gadget -> unit)
  end
  let () =
    let visitor = new visitor
      ~visit_gadget: (fun gadget ->
                        Printf.printf "Found gadget: %s\n" gadget#name)
      ~visit_widget: (fun widget ->
                        Printf.printf "Found widget: %s\n" widget#name) in
    List.iter
      (fun obj -> obj#accept visitor)
      [new widget "a"; new gadget "b"; new widget "c"]
  
  (* Yet another solution would be to rethink your design in terms of
     variants and pattern matching. *)
  

