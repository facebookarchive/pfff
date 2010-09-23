(* ********************************************************************** *)
(* Copying Filehandles *)
(* ********************************************************************** *)
let pleac_Copying_Filehandles () = 
  #load "unix.cma";;
  
  let () =
    (* Take copies of the file descriptors. *)
    let oldout = Unix.dup Unix.stdout in
    let olderr = Unix.dup Unix.stderr in
  
    (* Redirect stdout and stderr. *)
    let output =
      Unix.openfile
        "/tmp/program.out"
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
        0o666 in
    Unix.dup2 output Unix.stdout;
    Unix.close output;
    let copy = Unix.dup Unix.stdout in
    Unix.dup2 copy Unix.stderr;
    Unix.close copy;
  
    (* Run the program. *)
    ignore (Unix.system joe_random_process);
  
    (* Close the redirected file handles. *)
    Unix.close Unix.stdout;
    Unix.close Unix.stderr;
  
    (* Restore stdout and stderr. *)
    Unix.dup2 oldout Unix.stdout;
    Unix.dup2 olderr Unix.stderr;
  
    (* Avoid leaks by closing the independent copies. *)
    Unix.close oldout;
    Unix.close olderr
  

