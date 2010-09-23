(* ********************************************************************** *)
(* Reading or Writing to Another Program *)
(* ********************************************************************** *)
let pleac_Reading_or_Writing_to_Another_Program () = 
  #load "unix.cma";;
  
  (*-----------------------------*)
  
  (* Handle each line in the output of a process. *)
  let () =
    let readme = Unix.open_process_in "program arguments" in
    let rec loop line =
      (* ... *)
      loop (input_line readme) in
    try loop (input_line readme)
    with End_of_file -> ignore (Unix.close_process_in readme)
  
  (*-----------------------------*)
  
  (* Write to the input of a process. *)
  let () =
    let writeme = Unix.open_process_out "program arguments" in
    output_string writeme "data\n";
    ignore (Unix.close_process_out writeme)
  
  (*-----------------------------*)
  
  (* Wait for a process to complete. *)
  let () =
    (* child goes to sleep *)
    let f = Unix.open_process_in "sleep 100000" in
    (* and parent goes to lala land *)
    ignore (Unix.close_process_in f);
    ignore (Unix.wait ())
  
  (*-----------------------------*)
  
  let () =
    let writeme = Unix.open_process_out "program args" in
    (* program will get hello\n on STDIN *)
    output_string writeme "hello\n";
    (* program will get EOF on STDIN *)
    ignore (Unix.close_process_out writeme)
  
  (*-----------------------------*)
  
  (* Redirect standard output to the pager. *)
  let () =
    let pager =
      try Sys.getenv "PAGER" (* XXX: might not exist *)
      with Not_found -> "/usr/bin/less" in
    let reader, writer = Unix.pipe () in
    match Unix.fork () with
      | 0 ->
          Unix.close writer;
          Unix.dup2 reader Unix.stdin;
          Unix.close reader;
          Unix.execvp pager [| pager |]
      | pid ->
          Unix.close reader;
          Unix.dup2 writer Unix.stdout;
          Unix.close writer
  
  (* Do something useful that writes to standard output, then
     close the stream and wait for the pager to finish. *)
  let () =
    (* ... *)
    close_out stdout;
    ignore (Unix.wait ())
  

