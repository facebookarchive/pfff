(* ********************************************************************** *)
(* Replacing the Current Program with a Different One *)
(* ********************************************************************** *)
let pleac_Replacing_the_Current_Program_with_a_Different_One () = 
  #load "unix.cma";;
  (* Transfer control to the shell to run another program. *)
  let () = Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; "archive *.data" |]
  (* Transfer control directly to another program in the path. *)
  let () = Unix.execvp "archive" [| "archive"; "accounting.data" |]
  

