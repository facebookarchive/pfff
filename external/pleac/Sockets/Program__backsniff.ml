(* ********************************************************************** *)
(* Program: backsniff *)
(* ********************************************************************** *)
let pleac_Program__backsniff () = 
  Oct  4 11:01:16 pedro sniffer: Connection from 10.0.0.4 to 10.0.0.1:echo
  
  (*-----------------------------*)
  
  echo stream tcp nowait nobody /usr/bin/ocaml ocaml /path/to/backsniff.ml
  
  (*-----------------------------*)
  
  (* backsniff - log attempts to connect to particular ports *)
  #load "unix.cma";;
  
  (* This recipe uses syslog-ocaml, which is available at:
     http://www.cs.cmu.edu/~ecc/software.html *)
  #directory "+syslog";;
  #load "syslog.cma";;
  
  (* identify my port and address *)
  let sockname =
    try Unix.getsockname Unix.stdin
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "Couldn't identify myself: %s\n%!"
        (Unix.error_message e);
      exit 1
  let iaddr, port =
    match sockname with
      | Unix.ADDR_INET (iaddr, port) -> iaddr, port
      | _ -> assert false
  let my_address = Unix.string_of_inet_addr iaddr
  
  (* get a name for the service *)
  let service =
    try (Unix.getservbyport port "tcp").Unix.s_name
    with Not_found -> string_of_int port
  
  (* now identify remote address *)
  let sockname =
    try Unix.getpeername Unix.stdin
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "Couldn't identify other end: %s\n%!"
        (Unix.error_message e);
      exit 1
  let iaddr, port =
    match sockname with
      | Unix.ADDR_INET (iaddr, port) -> iaddr, port
      | _ -> assert false
  let ex_address = Unix.string_of_inet_addr iaddr
  
  (* and log the information *)
  let () =
    let log = Syslog.openlog ~flags:[] ~facility:`LOG_DAEMON "sniffer" in
    Syslog.syslog log `LOG_NOTICE
      (Printf.sprintf "Connection from %s to %s:%s\n"
         ex_address my_address service);
    Syslog.closelog log;
    exit 0
  

