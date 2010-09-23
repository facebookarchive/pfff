(* ********************************************************************** *)
(* Writing a TCP Server *)
(* ********************************************************************** *)
let pleac_Writing_a_TCP_Server () = 
  
  (* Writing a TCP Server *)
  (* Run this and then telnet <machinename> 1027 *)
  
  #load "unix.cma" ;;
  open Unix ;;
  
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  
  (* so we can restart our server quickly *)
  setsockopt server_sock SO_REUSEADDR true ;
  
  (* build up my socket address *)
  let address = (gethostbyname(gethostname())).h_addr_list.(0) in
  bind server_sock (ADDR_INET (address, 1029)) ;
  
  (* Listen on the socket. Max of 10 incoming connections. *)
  listen server_sock 10 ;
  
  (* accept and process connections *)
  while true do
  	let (client_sock, client_addr) = accept server_sock in
  	let str = "Hello\n" in
  	let len = String.length str in
  	let x = send client_sock str 0 len [] in
  	shutdown client_sock SHUTDOWN_ALL
  	done ;;
  

