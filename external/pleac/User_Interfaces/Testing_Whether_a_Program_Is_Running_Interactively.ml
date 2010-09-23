(* ********************************************************************** *)
(* Testing Whether a Program Is Running Interactively *)
(* ********************************************************************** *)
let pleac_Testing_Whether_a_Program_Is_Running_Interactively () = 
  #load "unix.cma";;
  
  let i_am_interactive () =
    Unix.isatty Unix.stdin && Unix.isatty Unix.stdout
  
  let () =
    try
      while true do
        if i_am_interactive ()
        then print_string "Prompt: ";
        let line = read_line () in
        if line = "" then raise End_of_file;
        (* do something with the line *)
      done
    with End_of_file -> ()
  

