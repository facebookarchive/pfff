(* ********************************************************************** *)
(* Making Perl Report Filenames in Errors *)
(* ********************************************************************** *)
let pleac_Making_Perl_Report_Filenames_in_Errors () = 
  #load "unix.cma";;
  
  open Unix
  
  (* Raises an exception on failure. *)
  let file = openfile filename [ O_RDONLY ] 0o640
  
  exception ErrString of string
  
  let file =
    try openfile filename [ O_RDONLY ] 0o640
    with Unix_error (e, f, n) ->
      raise (ErrString
               (Printf.sprintf "Could not open %s for read: %s"
                  n (error_message e)))
  

