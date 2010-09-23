(* ********************************************************************** *)
(* Catching Ctrl-C *)
(* ********************************************************************** *)
let pleac_Catching_Ctrl_C () = 
  let () =
    (* ignore signal INT *)
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
  
    (* install signal handler *)
    let rec tsktsk signal =
      Sys.set_signal Sys.sigint (Sys.Signal_handle tsktsk);
      print_endline "\x07The long habit of living indisposeth us for dying." in
    Sys.set_signal Sys.sigint (Sys.Signal_handle tsktsk)
  

