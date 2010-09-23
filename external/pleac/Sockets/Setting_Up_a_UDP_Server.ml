(* ********************************************************************** *)
(* Setting Up a UDP Server *)
(* ********************************************************************** *)
let pleac_Setting_Up_a_UDP_Server () = 
  #load "unix.cma";;
  let () =
    begin
      try
        Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_any, server_port));
      with Unix.Unix_error (e, _, _) ->
        Printf.eprintf "Couldn't be a udp server on port %d: %s\n"
          server_port (Unix.error_message e);
        exit 1
    end;
    let him = String.create max_to_read in
    while true do
      ignore (Unix.recvfrom socket him 0 max_to_read []);
      (* do something *)
    done
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* udpqotd - UDP message server *)
  #load "unix.cma";;
  
  let maxlen = 1024
  let portno = 5151
  
  let sock =
    Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  
  let () =
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, portno));
    Printf.printf "Awaiting UDP messages on port %d\n%!" portno
  
  let oldmsg = ref "This is the starting message."
  
  let () =
    let newmsg = String.create maxlen in
    while true do
      let newmsg, hishost, sockaddr =
        match Unix.recvfrom sock newmsg 0 maxlen [] with
          | len, (Unix.ADDR_INET (addr, port) as sockaddr) ->
              String.sub newmsg 0 len,
              (Unix.gethostbyaddr addr).Unix.h_name,
              sockaddr
          | _ -> assert false in
      Printf.printf "Client %s said ``%s''\n%!" hishost newmsg;
      ignore
        (Unix.sendto sock !oldmsg 0 (String.length !oldmsg) [] sockaddr);
      oldmsg := Printf.sprintf "[%s] %s" hishost newmsg
    done
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* udpmsg - send a message to the udpqotd server *)
  #load "unix.cma";;
  
  let maxlen  = 1024
  let portno  = 5151
  let timeout = 5
  
  let server_host, msg =
    match Array.to_list Sys.argv with
      | _ :: head :: tail -> head, String.concat " " tail
      | _ ->
          Printf.eprintf "Usage: %s server_host msg ...\n" Sys.argv.(0);
          exit 1
  
  let sock =
    Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  
  let sockaddr =
    let addr = (Unix.gethostbyname server_host).Unix.h_addr_list.(0) in
    Unix.ADDR_INET (addr, portno)
  
  let handle_alarm signal =
    Printf.eprintf "recv from %s timed out after %d seconds.\n"
      server_host timeout;
    exit 1
  
  let () =
    ignore (Unix.sendto sock msg 0 (String.length msg) [] sockaddr);
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle handle_alarm);
    ignore (Unix.alarm timeout);
    let msg = String.create maxlen in
    let msg, hishost =
      match Unix.recvfrom sock msg 0 maxlen [] with
        | len, Unix.ADDR_INET (addr, port) ->
            String.sub msg 0 len,
            (Unix.gethostbyaddr addr).Unix.h_name
        | _ -> assert false in
    ignore (Unix.alarm 0);
    Printf.printf "Server %s responded ``%s''\n" hishost msg
  

