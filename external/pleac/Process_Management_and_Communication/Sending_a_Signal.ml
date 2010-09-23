(* ********************************************************************** *)
(* Sending a Signal *)
(* ********************************************************************** *)
let pleac_Sending_a_Signal () = 
  #load "unix.cma";;
  let () =
    (* send pid a signal 9 *)
    Unix.kill pid 9;
    (* send whole job a signal 1 *)
    Unix.kill pgrp (-1);
    (* send myself a SIGUSR1 *)
    Unix.kill (Unix.getpid ()) Sys.sigusr1;
    (* send a SIGHUP to processes in pids *)
    List.iter (fun pid -> Unix.kill pid Sys.sighup) pids
  
  (*-----------------------------*)
  
  (* Use kill with pseudo-signal 0 to see if process is alive. *)
  let () =
    try
      Unix.kill minion 0;
      Printf.printf "%d is alive!\n" minion
    with
      | Unix.Unix_error (Unix.EPERM, _, _) -> (* changed uid *)
          Printf.printf "%d has escaped my control!\n" minion
      | Unix.Unix_error (Unix.ESRCH, _, _) ->
          Printf.printf "%d is deceased.\n" (* or zombied *) minion
      | e ->
          Printf.printf "Odd; I couldn't check on the status of %d: %s\n"
            minion
            (Printexc.to_string e)
  

