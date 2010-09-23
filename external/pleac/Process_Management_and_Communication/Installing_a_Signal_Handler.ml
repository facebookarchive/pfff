(* ********************************************************************** *)
(* Installing a Signal Handler *)
(* ********************************************************************** *)
let pleac_Installing_a_Signal_Handler () = 
  let () =
    (* call got_sig_quit for every SIGQUIT *)
    Sys.set_signal Sys.sigquit (Sys.Signal_handle got_sig_quit);
    (* call got_sig_pipe for every SIGPIPE *)
    Sys.set_signal Sys.sigpipe (Sys.Signal_handle got_sig_pipe);
    (* increment ouch for every SIGINT *)
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> incr ouch));
    (* ignore the signal INT *)
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    (* restore default STOP signal handling *)
    Sys.set_signal Sys.sigstop Sys.Signal_default
  

