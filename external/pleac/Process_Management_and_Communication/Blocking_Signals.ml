(* ********************************************************************** *)
(* Blocking Signals *)
(* ********************************************************************** *)
let pleac_Blocking_Signals () = 
  #load "unix.cma";;
  
  (* define the signals to block *)
  let sigset = [Sys.sigint; Sys.sigkill]
  
  let () =
    (* block signals *)
    let old_sigset = Unix.sigprocmask Unix.SIG_BLOCK sigset in
  
    (* ... *)
  
    (* unblock signals *)
    (* the original recipe uses SIG_UNBLOCK, but that doesn't seem right... *)
    ignore (Unix.sigprocmask Unix.SIG_SETMASK old_sigset)
  

