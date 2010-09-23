(* ********************************************************************** *)
(* Reading from the Keyboard *)
(* ********************************************************************** *)
let pleac_Reading_from_the_Keyboard () = 
  #load "unix.cma";;
  
  let with_cbreak f x =
    let term_init = Unix.tcgetattr Unix.stdin in
    let term_cbreak = { term_init with Unix.c_icanon = false } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term_cbreak;
    try
      let result = f x in
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term_init;
      result
    with e ->
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term_init;
      raise e
  
  let key = with_cbreak input_char stdin
  
  (*-----------------------------*)
  
  (* sascii - Show ASCII values for keypresses *)
  let sascii () =
    while true do
      let char = Char.code (input_char stdin) in
      Printf.printf " Decimal: %d\tHex: %x\n" char char;
      flush stdout
    done
  let () =
    print_endline
      "Press keys to see their ASCII values.  Use Ctrl-C to quit.";
    with_cbreak sascii ()
  

