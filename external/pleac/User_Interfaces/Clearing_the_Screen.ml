(* ********************************************************************** *)
(* Clearing the Screen *)
(* ********************************************************************** *)
let pleac_Clearing_the_Screen () = 
  #load "unix.cma";;
  
  (* Run the clear command to clear the screen. *)
  let () = ignore (Sys.command "clear")
  
  (* Save the output to a string to avoid running a process each time. *)
  let clear =
    try
      let proc = Unix.open_process_in "clear" in
      try
        let chars = input_line proc in
        ignore (Unix.close_process_in proc);
        chars
      with e -> ignore (Unix.close_process_in proc); ""
    with _ -> ""
  let () = print_string clear
  

