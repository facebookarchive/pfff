(* ********************************************************************** *)
(* Delaying use Until Run Time *)
(* ********************************************************************** *)
let pleac_Delaying_use_Until_Run_Time () = 
  (* Registry.ml *)
  let (registry : (string, unit -> unit) Hashtbl.t) = Hashtbl.create 32
  
  (* SomeModule.ml *)
  let say_hello () = print_endline "Hello, world!"
  let () = Hashtbl.replace Registry.registry "say_hello" say_hello
  
  (* Main program *)
  let filename = "SomeModule.cmo"
  let funcname = "say_hello"
  let () =
    Dynlink.init ();
    (try Dynlink.loadfile filename
     with Dynlink.Error e -> failwith (Dynlink.error_message e));
    (Hashtbl.find Registry.registry funcname) ()
  
  (* Note that the Dynlink module currently supports dynamic loading of
     bytecode modules only. There is a project to add support for dynamic
     loading of native code which has been merged with OCaml's CVS HEAD.
     Details are available at http://alain.frisch.fr/natdynlink.html *)
  

