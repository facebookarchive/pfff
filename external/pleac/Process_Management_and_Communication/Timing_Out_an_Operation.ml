(* ********************************************************************** *)
(* Timing Out an Operation *)
(* ********************************************************************** *)
let pleac_Timing_Out_an_Operation () = 
  #load "unix.cma";;
  let () =
    Sys.set_signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> failwith "timeout"));
  
    ignore (Unix.alarm 3600);
    try
      (* long-time operations here *)
      ignore (Unix.alarm 0)
    with
      | Failure "timeout" ->
          (* timed out; do what you will here *)
          ()
      | e ->
          (* clear the still-pending alarm *)
          ignore (Unix.alarm 0);
          (* propagate unexpected exception *)
          raise e
  

