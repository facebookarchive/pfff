(* ********************************************************************** *)
(* Calling Methods Indirectly *)
(* ********************************************************************** *)
let pleac_Calling_Methods_Indirectly () = 
  (* Create a hashtable mapping method names to method calls. *)
  let methods = Hashtbl.create 3
  let () =
    Hashtbl.replace methods "run"   (fun obj -> obj#run ());
    Hashtbl.replace methods "start" (fun obj -> obj#start ());
    Hashtbl.replace methods "stop"  (fun obj -> obj#stop ())
  
  (* Call the three methods on the object by name. *)
  let () =
    List.iter
      (fun m -> (Hashtbl.find methods m) obj)
      ["start"; "run"; "stop"]
  
  (* You can alias a method as long as it takes at least one argument. *)
  let () =
    let meth = obj#run in
    (* ... *)
    meth ()
  

