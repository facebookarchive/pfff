(* ********************************************************************** *)
(* Flushing Output *)
(* ********************************************************************** *)
let pleac_Flushing_Output () = 
  (* OCaml automatically flushes after calling these functions: *)
  let () =
    print_endline "I get flushed.";
    print_newline (); (* Me too! *)
    prerr_endline "So do I.";
    prerr_newline () (* As do I. *)
  
  (* The Printf functions allow a format specifier of "%!" to trigger
     an immediate flush. *)
  let () = Printf.printf "I flush %s%! and %s!\n%!" "here" "there"
  
  (*-----------------------------*)
  
  (* seeme - demo stdio output buffering *)
  #load "unix.cma";;
  let () =
    output_string stdout "Now you don't see it...";
    Unix.sleep 2;
    print_endline "now you do"
  
  (*-----------------------------*)
  
  (* A channel can be explicitly flushed: *)
  let () = flush stderr
  
  (* All channels can be flushed at once (errors are ignored): *)
  let () = flush_all ()
  
  (* Closing a channel flushes automatically: *)
  let () =
    output_string stdout "I get written.\n";
    close_out stdout
  
  (* Calls to exit result in a flush_all, and exit is always called at
     termination even if an error occurs. *)
  let () =
    output_string stderr "Bye!\n";
    exit 0
  

