(* ********************************************************************** *)
(* Locking a File *)
(* ********************************************************************** *)
let pleac_Locking_a_File () = 
  #load "unix.cma";;
  
  let descr = Unix.openfile path [Unix.O_RDWR] 0o664
  
  let () =
    Unix.lockf descr Unix.F_LOCK 0;
    (* update file, then ... *)
    Unix.close descr
  
  let () =
    try Unix.lockf descr Unix.F_TLOCK 0
    with Unix.Unix_error (error, _, _) ->
      Printf.eprintf
        "can't immediately write-lock the file (%s), blocking ...\n"
        (Unix.error_message error);
      flush stderr;
      Unix.lockf descr Unix.F_LOCK 0
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  
  let descr = Unix.openfile "numfile" [Unix.O_RDWR; Unix.O_CREAT] 0o664
  
  let () =
    Unix.lockf descr Unix.F_LOCK 0;
    (* Now we have acquired the lock, it's safe for I/O *)
    let num =
      try int_of_string (input_line (Unix.in_channel_of_descr descr))
      with _ -> 0 in
    ignore (Unix.lseek descr 0 Unix.SEEK_SET);
    Unix.ftruncate descr 0;
    let out = Unix.out_channel_of_descr descr in
    output_string out (string_of_int (num + 1));
    output_string out "\n";
    flush out;
    Unix.close descr
  

