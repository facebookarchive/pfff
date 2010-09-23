(* ********************************************************************** *)
(* Writing a TCP Client *)
(* ********************************************************************** *)
let pleac_Writing_a_TCP_Client () = 
  
  (* For real applications you should the SMTP module in Ocamlnet. *)
  open Unix
  
  let sock_send sock str =
  	    let len = String.length str in
      send sock str 0 len []
  
  let sock_recv sock maxlen =
      let str = String.create maxlen in
      let recvlen = recv sock str 0 maxlen [] in
      String.sub str 0 recvlen
  
  let client_sock = socket PF_INET SOCK_STREAM 0 in
  let hentry = gethostbyname "coltrane" in
  connect client_sock (ADDR_INET (hentry.h_addr_list.(0), 25)) ; (* SMTP *)
  
  sock_recv client_sock 1024 ;
  
  sock_send client_sock "mail from: <pleac@localhost>\n" ;
  sock_recv client_sock 1024 ;
  
  sock_send client_sock "rcpt to: <erikd@localhost>\n" ;
  sock_recv client_sock 1024;
  
  sock_send client_sock "data\n" ;
  sock_recv client_sock 1024 ;
  
  sock_send client_sock "From: Ocaml whiz\nSubject: Ocaml rulez!\n\nYES!\n.\n" ;
  sock_recv client_sock 1024 ;
  
  close client_sock ;;
  
  

