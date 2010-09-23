(* ********************************************************************** *)
(* Writing a Multi-Homed Server *)
(* ********************************************************************** *)
let pleac_Writing_a_Multi_Homed_Server () = 
  #load "unix.cma";;
  
  let server =
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM
      (Unix.getprotobyname "tcp").Unix.p_proto
  
  let () =
    Unix.setsockopt server Unix.SO_REUSEADDR true;
    Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, server_port));
    Unix.listen server 10;
  
    (* accept loop *)
    while true do
      let client, sockaddr = Unix.accept server in
      match Unix.getsockname client with
        | Unix.ADDR_INET (addr, port) ->
            print_endline (Unix.string_of_inet_addr addr)
        | _ -> assert false
    done
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  
  let port = 4269                 (* port to bind to *)
  let host = "specific.host.com"  (* virtual host to listen on *)
  
  let server =
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM
      (Unix.getprotobyname "tcp").Unix.p_proto
  
  let () =
    let addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
    Unix.bind server (Unix.ADDR_INET (addr, port));
    Unix.listen server 10;
    while true do
      let client, sockaddr = Unix.accept server in
      (* ... *)
      ()
    done
  

