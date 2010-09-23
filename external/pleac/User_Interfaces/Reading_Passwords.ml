(* ********************************************************************** *)
(* Reading Passwords *)
(* ********************************************************************** *)
let pleac_Reading_Passwords () = 
  #load "unix.cma";;
  
  (* Thanks to David Mentre, Remi Vanicat, and David Brown's posts on
     caml-list. Works on Unix only, unfortunately, due to tcsetattr. *)
  let read_password () =
    let term_init = Unix.tcgetattr Unix.stdin in
    let term_no_echo = { term_init with Unix.c_echo = false } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term_no_echo;
    try
      let password = read_line () in
      print_newline ();
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH term_init;
      password
    with e ->
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH term_init;
      raise e
  
  let () =
    print_string "Enter your password: ";
    let password = read_password () in
    Printf.printf "You said: %s\n" password
  

