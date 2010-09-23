(* ********************************************************************** *)
(* Using UNIX Domain Sockets *)
(* ********************************************************************** *)
let pleac_Using_UNIX_Domain_Sockets () = 
  #load "unix.cma";;
  
  (* Create a Unix domain socket server - you can also use SOCK_STREAM. *)
  let server = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0
  let () = try Unix.unlink "/tmp/mysock" with Unix.Unix_error _ -> ()
  let () = Unix.bind server (Unix.ADDR_UNIX "/tmp/mysock")
  
  (* Create a Unix domain socket client - you can also use SOCK_STREAM. *)
  let client = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0
  let () = Unix.connect client (Unix.ADDR_UNIX "/tmp/mysock")
  

