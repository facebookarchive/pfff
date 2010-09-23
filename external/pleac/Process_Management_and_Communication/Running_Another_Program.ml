(* ********************************************************************** *)
(* Running Another Program *)
(* ********************************************************************** *)
let pleac_Running_Another_Program () = 
  (* Run a simple command and retrieve its result code. *)
  let status = Sys.command ("vi " ^ myfile)
  
  (*-----------------------------*)
  
  (* Use the shell to perform redirection. *)
  let _ = Sys.command "cmd1 args | cmd2 | cmd3 >outfile"
  let _ = Sys.command "cmd args <infile >outfile 2>errfile"
  
  (*-----------------------------*)
  
  (* Run a command, handling its result code or signal. *)
  #load "unix.cma";;
  let () =
    match Unix.system command with
      | Unix.WEXITED status ->
          Printf.printf "program exited with status %d\n" status
      | Unix.WSIGNALED signal ->
          Printf.printf "program killed by signal %d\n" signal
      | Unix.WSTOPPED signal ->
          Printf.printf "program stopped by signal %d\n" signal
  
  (*-----------------------------*)
  
  (* Run a command while blocking interrupt signals. *)
  #load "unix.cma";;
  let () =
    match Unix.fork () with
      | 0 ->
          (* child ignores INT and does its thing *)
          Sys.set_signal Sys.sigint Sys.Signal_ignore;
          Unix.execv "/bin/sleep" [| "/bin/sleep"; "10" |]
      | pid ->
          (* parent catches INT and berates user *)
          Sys.set_signal Sys.sigint
            (Sys.Signal_handle
              (fun _ -> print_endline "Tsk tsk, no process interruptus"));
          let running = ref true in
          while !running do
            try (ignore (Unix.waitpid [] pid); running := false)
            with Unix.Unix_error _ -> ()
          done;
          Sys.set_signal Sys.sigint Sys.Signal_default
  
  (*-----------------------------*)
  
  (* Run a command with a different name in the process table. *)
  #load "unix.cma";;
  let shell = "/bin/tcsh"
  let () =
    match Unix.fork () with
      | 0 -> Unix.execv shell [| "-csh" |] (* pretend it's a login shell *)
      | pid -> ignore (Unix.waitpid [] pid)
  

