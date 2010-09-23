(* ********************************************************************** *)
(* Determining the Caller's Package *)
(* ********************************************************************** *)
let pleac_Determining_the_Caller_s_Package () = 
  (* This is very difficult to do in OCaml due to the lack of reflection
     capabilities. Determining the current module name is reasonably easy,
     however, by using the __FILE__ constant exposed by camlp4's macro
     extensions. *)
  
  (*pp camlp4of *)
  let __MODULE__ = String.capitalize (Filename.chop_extension __FILE__)
  let () = Printf.printf "I am in module %s\n" __MODULE__
  

