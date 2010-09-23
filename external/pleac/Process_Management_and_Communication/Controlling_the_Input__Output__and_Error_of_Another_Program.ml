(* ********************************************************************** *)
(* Controlling the Input, Output, and Error of Another Program *)
(* ********************************************************************** *)
let pleac_Controlling_the_Input__Output__and_Error_of_Another_Program () = 
  #load "unix.cma";;
  let () =
    let proc =
      Unix.open_process_in
        ("(" ^ cmd ^ " | sed -e 's/^/stdout: /' ) 2>&1") in
    try
      while true do
        let line = input_line proc in
        if String.length line >= 8
          && String.sub line 0 8 = "stdout: "
        then Printf.printf "STDOUT: %s\n"
          (String.sub line 8 (String.length line - 8))
        else Printf.printf "STDERR: %s\n" line
      done
    with End_of_file ->
      ignore (Unix.close_process_in proc)
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* cmd3sel - control all three of kids in, out, and error. *)
  #load "unix.cma";;
  
  let cmd = "grep vt33 /none/such - /etc/termcap"
  let cmd_out, cmd_in, cmd_err = Unix.open_process_full cmd [| |]
  
  let () =
    output_string cmd_in "This line has a vt33 lurking in it\n";
    close_out cmd_in;
    let cmd_out_descr = Unix.descr_of_in_channel cmd_out in
    let cmd_err_descr = Unix.descr_of_in_channel cmd_err in
    let selector = ref [cmd_err_descr; cmd_out_descr] in
    while !selector <> [] do
      let can_read, _, _ = Unix.select !selector [] [] 1.0 in
      List.iter
        (fun fh ->
           try
             if fh = cmd_err_descr
             then Printf.printf "STDERR: %s\n" (input_line cmd_err)
             else Printf.printf "STDOUT: %s\n" (input_line cmd_out)
           with End_of_file ->
             selector := List.filter (fun fh' -> fh <> fh') !selector)
        can_read
    done;
    ignore (Unix.close_process_full (cmd_out, cmd_in, cmd_err))
  

