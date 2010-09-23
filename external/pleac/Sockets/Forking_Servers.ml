(* ********************************************************************** *)
(* Forking Servers *)
(* ********************************************************************** *)
let pleac_Forking_Servers () = 
  (* set up the socket SERVER, bind and listen ... *)
  #load "unix.cma";;
  
  let rec reaper signal =
    try while true do ignore (Unix.waitpid [Unix.WNOHANG] (-1)) done
    with Unix.Unix_error (Unix.ECHILD, _, _) -> ();
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)
  
  let () =
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)
  
  let () =
    while true do
      try
        let (client, addr) = Unix.accept server in
        let pid = Unix.fork () in
        if pid = 0 then                   (* parent *)
          begin
            Unix.close server;            (* no use to child *)
            (* ... do something *)
            exit 0                        (* child leaves *)
          end
        else
          begin
            Unix.close client             (* no use to parent *)
          end
      with Unix.Unix_error (Unix.EINTR, _, _) -> ()
    done
  

