(* ********************************************************************** *)
(* Pre-Forking Servers *)
(* ********************************************************************** *)
let pleac_Pre_Forking_Servers () = 
  #!/usr/bin/ocaml
  (* preforker - server who forks first *)
  #load "unix.cma";;
  
  (* global variables *)
  let prefork = 5
  let max_clients_per_child = 5
  module PidSet = Set.Make(struct type t = int let compare = compare end)
  let children = ref PidSet.empty
  
  (* takes care of dead children *)
  let rec reaper _ =
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper);
    match Unix.wait ()
    with (pid, _) -> children := PidSet.remove pid !children
  
  (* signal handler for SIGINT *)
  let rec huntsman _ =
    (* we're going to kill our children *)
    Sys.set_signal Sys.sigchld Sys.Signal_ignore;
    PidSet.iter
      (fun pid ->
         try Unix.kill Sys.sigint pid with Unix.Unix_error _ -> ())
      !children;
    (* clean up with dignity *)
    exit 0
  
  let make_new_child server =
    (* block signal for fork *)
    let sigset = [Sys.sigint] in
    ignore (Unix.sigprocmask Unix.SIG_BLOCK sigset);
  
    match Unix.fork () with
      | 0 ->
          (* Child can *not* return from this subroutine. *)
          (* make SIGINT kill us as it did before *)
          Sys.set_signal Sys.sigint Sys.Signal_default;
  
          (* unblock signals *)
          ignore (Unix.sigprocmask Unix.SIG_UNBLOCK sigset);
  
          (* handle connections until we've reached max_clients_per_child *)
          for i = 1 to max_clients_per_child do
            let (client, _) = Unix.accept server in
            (* do something with the connection *)
            ()
          done;
  
          (* tidy up gracefully and finish *)
  
          (* this exit is VERY important, otherwise the child will become
             a producer of more and more children, forking yourself into
             process death. *)
          exit 0
      | pid ->
          (* Parent records the child's birth and returns. *)
          ignore (Unix.sigprocmask Unix.SIG_UNBLOCK sigset);
          children := PidSet.add pid !children
  
  let () =
    (* establish SERVER socket, bind and listen. *)
    let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt server Unix.SO_REUSEADDR true;
    Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, 6969));
    Unix.listen server 10;
  
    (* Fork off our children. *)
    for i = 1 to prefork do
      make_new_child server
    done;
  
    (* Install signal handlers. *)
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper);
    Sys.set_signal Sys.sigint (Sys.Signal_handle huntsman);
  
    (* And maintain the population. *)
    while true do
      (* wait for a signal (i.e., child's death) *)
      Unix.pause ();
      for i = (PidSet.cardinal !children) to (prefork - 1) do
        (* top up the child pool *)
        make_new_child server
      done
    done
  

