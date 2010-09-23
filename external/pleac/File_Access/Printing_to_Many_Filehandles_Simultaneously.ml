(* ********************************************************************** *)
(* Printing to Many Filehandles Simultaneously *)
(* ********************************************************************** *)
let pleac_Printing_to_Many_Filehandles_Simultaneously () = 
  (* Save your channels in a list and iterate through them normally. *)
  let () =
    List.iter
      (fun channel ->
         output_string channel stuff_to_print)
      channels
  
  (* For convenience, you can define a helper function and use currying. *)
  let write data channel = output_string channel data
  let () = List.iter (write stuff_to_print) channels
  
  (*-----------------------------*)
  
  (* Open a pipe to "tee". Requires a Unix environment. *)
  #load "unix.cma";;
  let () =
    let channel =
      Unix.open_process_out "tee file1 file2 file3 >/dev/null" in
    output_string channel "whatever\n";
    ignore (Unix.close_process_out channel)
  
  (*-----------------------------*)
  
  (* Redirect standard output to a tee. *)
  let () =
    let reader, writer = Unix.pipe () in
    match Unix.fork () with
      | 0 ->
          Unix.close writer;
          Unix.dup2 reader Unix.stdin;
          Unix.close reader;
          Unix.execvp "tee" [| "tee"; "file1"; "file2"; "file3" |]
      | pid ->
          Unix.close reader;
          Unix.dup2 writer Unix.stdout;
          Unix.close writer
  let () =
    print_endline "whatever";
    close_out stdout;
    ignore (Unix.wait ())
  

