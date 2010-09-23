(* ********************************************************************** *)
(* Writing a Signal Handler *)
(* ********************************************************************** *)
let pleac_Writing_a_Signal_Handler () = 
  let rec got_int _ =
    Sys.set_signal Sys.sigint (Sys.Signal_handle got_int);
    (* but not for SIGCHLD! *)
    (* ... *)
    ()
  
  (*-----------------------------*)
  
  let rec got_int _ =
    Sys.set_signal Sys.sigint Sys.Signal_default; (* or Signal_ignore *)
    failwith "interrupted"
  
  let () =
    Sys.set_signal Sys.sigint (Sys.Signal_handle got_int);
    try
      (* ... long-running code that you don't want to restart *)
      ()
    with Failure "interrupted" ->
      (* deal with the signal *)
      ()
  

