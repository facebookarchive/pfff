(* ********************************************************************** *)
(* Making a Daemon Server *)
(* ********************************************************************** *)
let pleac_Making_a_Daemon_Server () = 
  #load "unix.cma";;
  
  let () =
    (* for the paranoid *)
    (* Unix.handle_unix_error Unix.chroot "/var/daemon"; *)
  
    (* fork and let parent exit *)
    let pid = Unix.fork () in
    if pid > 0 then exit 0;
    
    (* create a new session and abandon the controlling process *)
    ignore (Unix.setsid ())
  
  (* flag indicating it is time to exit *)
  let time_to_die = ref false
  
  (* trap fatal signals *)
  let () =
    let signal_handler _ = time_to_die := true in
    List.iter
      (fun signal ->
         Sys.set_signal signal (Sys.Signal_handle signal_handler))
      [Sys.sigint; Sys.sigterm; Sys.sighup]
    (* trap or ignore Sys.sigpipe *)
  
  (* server loop *)
  let () =
    while not !time_to_die do
      (* ... *)
      ()
    done
  

