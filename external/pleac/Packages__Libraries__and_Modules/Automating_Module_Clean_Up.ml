(* ********************************************************************** *)
(* Automating Module Clean-Up *)
(* ********************************************************************** *)
let pleac_Automating_Module_Clean_Up () = 
  (* Use the built-in function, "at_exit", to schedule clean-up handlers
     to run when the main program exits. *)
  
  #load "unix.cma";;
  
  let logfile = "/tmp/mylog"
  let lf = open_out logfile
  
  let logmsg msg =
    Printf.fprintf lf "%s %d: %s\n%!"
      Sys.argv.(0) (Unix.getpid ()) msg
  
  (* Setup code. *)
  let () =
    logmsg "startup"
  
  (* Clean-up code. *)
  let () =
    at_exit
      (fun () ->
         logmsg "shutdown";
         close_out lf)
  

