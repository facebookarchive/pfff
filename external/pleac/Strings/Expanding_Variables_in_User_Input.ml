(* ********************************************************************** *)
(* Expanding Variables in User Input *)
(* ********************************************************************** *)
let pleac_Expanding_Variables_in_User_Input () = 
  (* As far as I know there is no way to do this in OCaml due to
     type-safety contraints built into the OCaml compiler -- it may be
     feasible with *much* juju, but don't expect to see this anytime
     soon...
  
     If you don't mind supplying a data structure rather than capturing
     local variables, you can use Buffer.add_substitute to get a similar
     effect. *)
  
  let buffer = Buffer.create 16
  let vars = [("debt", "$700 billion")]
  let () =
    Buffer.add_substitute buffer
      (fun name -> List.assoc name vars)
      "You owe $debt to me.";
    print_endline (Buffer.contents buffer)
  

