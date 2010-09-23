(* ********************************************************************** *)
(* Avoiding Zombie Processes *)
(* ********************************************************************** *)
let pleac_Avoiding_Zombie_Processes () = 
  #load "unix.cma";;
  
  let () =
    Sys.set_signal Sys.sigchld Sys.Signal_ignore
  
  (*-----------------------------*)
  
  let rec reaper signal =
    try while true do ignore (Unix.waitpid [Unix.WNOHANG] (-1)) done
    with Unix.Unix_error (Unix.ECHILD, _, _) -> ();
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)
  
  let () =
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)
  
  (*-----------------------------*)
  
  let rec reaper signal =
    begin try
      let pid, status = Unix.waitpid [Unix.WNOHANG] (-1) in begin
        match status with
          | Unix.WEXITED _ ->
              Printf.printf "Process %d exited.\n" pid
          | _ ->
              Printf.printf "False alarm on %d.\n" pid;
      end;
      reaper signal
    with Unix.Unix_error (Unix.ECHILD, _, _) ->
      () (* No child waiting. Ignore it. *)
    end;
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)
  
  let () =
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)
  

