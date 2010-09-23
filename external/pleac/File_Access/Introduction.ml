(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  #load "str.cma";;
  
  (* Print all lines that contain the word "blue" in the input file
     /usr/local/widgets/data to stdout. *)
  let () =
    let in_channel = open_in "/usr/local/widgets/data" in
    try
      while true do
        let line = input_line in_channel in
        try
          ignore (Str.search_forward (Str.regexp_string "blue") line 0);
          print_endline line
        with Not_found -> ()
      done
    with End_of_file ->
      close_in in_channel
  
  (*-----------------------------*)
  
  let () =
    let regexp = Str.regexp ".*[0-9]" in
    try
      while true do
        (* reads from stdin *)
        let line = input_line stdin in
        (* writes to stderr *)
        if not (Str.string_match regexp line 0)
        then prerr_endline "No digit found.";
        (* writes to stdout *)
        Printf.printf "Read: %s\n" line;
        flush stdout
      done
    with End_of_file ->
      close_out stdout
  
  (*-----------------------------*)
  
  (* Write to an output file the usual way. *)
  let () =
    let logfile = open_out "/tmp/log" in
    output_string logfile "Countdown initiated...\n";
    close_out logfile;
    print_endline "You have 30 seconds to reach minimum safety distance."
  
  (* Write to an output file using redirection. *)
  #load "unix.cma";;
  let () =
    let logfile = open_out "/tmp/log" in
    let old_descr = Unix.dup Unix.stdout in
    (* switch to logfile for output *)
    Unix.dup2 (Unix.descr_of_out_channel logfile) Unix.stdout;
    print_endline "Countdown initiated...";
    (* return to original output *)
    Unix.dup2 old_descr Unix.stdout;
    print_endline "You have 30 seconds to reach minimum safety distance."
  

