(* ********************************************************************** *)
(* Temporarily Overriding a Signal Handler *)
(* ********************************************************************** *)
let pleac_Temporarily_Overriding_a_Signal_Handler () = 
  let finally handler f x =
    let result = try f x with e -> handler (); raise e in handler (); result
  
  (* call f with signal behavior temporarily set *)
  let local_set_signal signal behavior f =
    let old_behavior = Sys.signal signal behavior in
    finally (fun () -> Sys.set_signal signal old_behavior) f ()
  
  (* the signal handler *)
  let rec ding _ =
    Sys.set_signal Sys.sigint (Sys.Signal_handle ding);
    prerr_endline "\x07Enter your name!"
  
  (* prompt for name, overriding SIGINT *)
  let get_name () =
    local_set_signal
      Sys.sigint (Sys.Signal_handle ding)
      (fun () ->
         print_string "Kindly Stranger, please enter your name: ";
         read_line ())
  

