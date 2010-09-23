(* ********************************************************************** *)
(* Closing a Socket After Forking *)
(* ********************************************************************** *)
let pleac_Closing_a_Socket_After_Forking () = 
  
  (* Closing a Socket After Forking *)
  
  (*-----------------------------*)
  shutdown sock SHUTDOWN_RECEIVE ;    (* I/we have stopped reading data *)
  shutdown sock SHUTDOWN_SEND ;       (* I/we have stopped writing data *)
  shutdown sock SHUTDOWN_ALL ;;       (* I/we have stopped using this socket *)
  
  (*-----------------------------*)
  (* Using the sock_send and sock_recv functions from above. *)
  
  sock_send sock "my request\n" ;    (* send some data *)
  shutdown sock SHUTDOWN_SEND ;      (* send eof; no more writing *)
  let answer = sock_recv sock 4096 ;; (* but you can still read *)
  

