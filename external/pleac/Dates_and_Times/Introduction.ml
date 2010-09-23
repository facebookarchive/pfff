(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  (*-----------------------------*)
  
  (* The unix module acts as a thin wrapper around the standard C
  ** Posix API. It comes standard with the Ocaml compiler but is
  ** not automatcially linked.
  ** If you are not using the command line interpreter, delete the
  ** the "#load" line
  *)
  
  #load "unix.cma" ;;
  open Unix ;;
  let t = Unix.localtime (Unix.time ());;
  
  Printf.printf "Today is day %d of the current year.\n" t.tm_yday ;;
  

