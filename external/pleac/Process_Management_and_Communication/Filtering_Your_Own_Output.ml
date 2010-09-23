(* ********************************************************************** *)
(* Filtering Your Own Output *)
(* ********************************************************************** *)
let pleac_Filtering_Your_Own_Output () = 
  #load "unix.cma";;
  
  (* Fork a process that calls f to post-process standard output. *)
  let push_output_filter f =
    let reader, writer = Unix.pipe () in
    match Unix.fork () with
      | 0 ->
          Unix.close writer;
          Unix.dup2 reader Unix.stdin;
          Unix.close reader;
          f ();
          exit 0
      | pid ->
          Unix.close reader;
          Unix.dup2 writer Unix.stdout;
          Unix.close writer
  
  (* Only display a certain number of lines of output. *)
  let head ?(lines=20) () =
    push_output_filter
      (fun () ->
         let lines = ref lines in
         try
           while !lines > 0 do
             print_endline (read_line ());
             decr lines
           done
         with End_of_file -> ())
  
  (* Prepend line numbers to each line of output. *)
  let number () =
    push_output_filter
      (fun () ->
         let line_number = ref 0 in
         try
           while true do
             let line = read_line () in
             incr line_number;
             Printf.printf "%d: %s\n" !line_number line
           done
         with End_of_file -> ())
  
  (* Prepend "> " to each line of output. *)
  let quote () =
    push_output_filter
      (fun () ->
         try
           while true do
             let line = read_line () in
             Printf.printf "> %s\n" line
           done
         with End_of_file -> ())
  
  let () =
    head ~lines:100 ();  (* push head filter on STDOUT *)
    number ();           (* push number filter on STDOUT *)
    quote ();            (* push quote filter on STDOUT *)
  
    (* act like /bin/cat *)
    begin
      try
        while true do
          print_endline (read_line ())
        done
      with End_of_file -> ()
    end;
  
    (* tell kids we're done--politely *)
    close_out stdout;
    ignore (Unix.waitpid [] (-1));
    exit 0
  

