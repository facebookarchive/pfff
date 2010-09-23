(* ********************************************************************** *)
(* Program: fwdport *)
(* ********************************************************************** *)
let pleac_Program__fwdport () = 
  #!/usr/bin/ocaml
  (* fwdport -- act as proxy forwarder for dedicated services *)
  
  #load "str.cma";;
  #load "unix.cma";;
  
  let children = Hashtbl.create 0    (* hash of outstanding child processes *)
  let remote = ref ""                (* whom we connect to on the outside *)
  let local = ref ""                 (* where we listen to on the inside *)
  let service = ref ""               (* our service name or port number *)
  let proxy_server = ref Unix.stdin  (* the socket we accept() from *)
  
  (* process command line switches *)
  let check_args () =
    Arg.parse
      [
        "-r",       Arg.Set_string remote,  "Remote host";
        "-remote",  Arg.Set_string remote,  "Remote host";
        "-l",       Arg.Set_string local,   "Local interface";
        "-local",   Arg.Set_string local,   "Local interface";
        "-s",       Arg.Set_string service, "Service";
        "-service", Arg.Set_string service, "Service";
      ]
      (fun s ->
         raise (Arg.Bad (Printf.sprintf "unexpected argument `%s'" s)))
      (Printf.sprintf "usage: %s [ -remote host ] [ -local interface ] [ -service service ]" Sys.argv.(0));
    if !remote = ""
    then (prerr_endline "Need remote"; exit 1);
    if !local = "" && !service = ""
    then (prerr_endline "Need local or service"; exit 1);
    if !local = ""
    then local := "localhost"
  
  let parse_host host =
    match Str.split (Str.regexp ":") host with
      | [] -> "", ""
      | host :: [] -> host, ""
      | host :: service :: _ -> host, service
  
  let resolve_host host =
    try (Unix.gethostbyname host).Unix.h_addr_list.(0)
    with Not_found ->
      Printf.eprintf "Host not found: %s\n" host;
      exit 1
  
  let resolve_service service =
    try int_of_string service
    with Failure _ ->
      try (Unix.getservbyname service "tcp").Unix.s_port
      with Not_found ->
        Printf.eprintf "Service not found: %s\n" service;
        exit 1
  
  (* begin our server *)
  let start_proxy () =
    try
      let proto = (Unix.getprotobyname "tcp").Unix.p_proto in
      let addr, port =
        match parse_host (!local ^ ":" ^ !service) with
          | host, service ->
              (resolve_host host,
               resolve_service service) in
      proxy_server := Unix.socket Unix.PF_INET Unix.SOCK_STREAM proto;
      Unix.setsockopt !proxy_server Unix.SO_REUSEADDR true;
      Unix.bind !proxy_server (Unix.ADDR_INET (addr, port));
      Unix.listen !proxy_server 128;
      Printf.printf "[Proxy server on %s initialized.]\n%!"
        (if !local <> "" then !local else !service)
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "Can't create proxy server: %s\n%!"
        (Unix.error_message e);
      exit 1
  
  (* helper function to produce a nice string in the form HOST:PORT *)
  let peerinfo sock =
    match Unix.getpeername sock with
      | Unix.ADDR_INET (addr, port) ->
          let hostinfo = Unix.gethostbyaddr addr in
          Printf.sprintf "%s:%d" hostinfo.Unix.h_name port
      | _ -> assert false
  
  (* somebody just died.  keep harvesting the dead until  *)
  (* we run out of them.  check how long they ran. *)
  let rec reaper signal =
    begin
      let result =
        try Some (Unix.waitpid [Unix.WNOHANG] (-1))
        with Unix.Unix_error (Unix.ECHILD, _, _) -> None in
      match result with
        | Some (child, status) when Hashtbl.mem children child ->
            let start = Hashtbl.find children child in
            let runtime = Unix.time () -. start in
            Printf.printf "Child %d ran %dm%fs\n%!"
              child
              (int_of_float (runtime /. 60.))
              (mod_float runtime 60.);
            Hashtbl.remove children child;
            reaper signal
        | Some (child, status) ->
            Printf.printf "Bizarre kid %d exited with %s\n%!"
              child
              (match status with
                 | Unix.WEXITED code ->
                     "code " ^ string_of_int code
                 | Unix.WSTOPPED signal
                 | Unix.WSIGNALED signal ->
                     "signal " ^ string_of_int signal);
            reaper signal
        | None -> ()
    end;
    (* If I had to choose between System V and 4.2, I'd resign. *)
    (* --Peter Honeyman *)
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)
  
  let service_clients () =
    (* harvest the moribund *)
    Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper);
  
    (* an accepted connection here means someone inside wants out *)
    while true do
      try
        begin
          let local_client = fst (Unix.accept !proxy_server) in
          let lc_info = peerinfo local_client in
          Printf.printf "[Connect from %s]\n%!" lc_info;
  
          let proto = (Unix.getprotobyname "tcp").Unix.p_proto in
          let addr, port =
            match parse_host (!remote ^ ":" ^ !service) with
              | host, service ->
                  (resolve_host host,
                   resolve_service service) in
          Printf.printf "[Connecting to %s...%!" !remote;
          let remote_server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM proto in
          Unix.connect remote_server (Unix.ADDR_INET (addr, port));
          Printf.printf "done]\n%!";
  
          let local_in = Unix.in_channel_of_descr local_client in
          let local_out = Unix.out_channel_of_descr local_client in
          let remote_in = Unix.in_channel_of_descr remote_server in
          let remote_out = Unix.out_channel_of_descr remote_server in
  
          match Unix.fork () with
            | 0 ->
                (* at this point, we are the forked child process dedicated *)
                (* to the incoming client.  but we want a twin to make i/o *)
                (* easier. *)
  
                Unix.close !proxy_server;  (* no use to slave *)
  
                (* now each twin sits around and ferries lines of data. *)
                (* see how simple the algorithm is when you can have *)
                (* multiple threads of control? *)
  
                (match Unix.fork () with
                   | 0 ->
                       (* this is the fork's child, the master's grandchild *)
                       (try
                            while true do
                              let line = input_line local_in in
                              Printf.fprintf remote_out "%s\n%!" line
                            done
                        with End_of_file ->
                          (* kill my twin cause we're done *)
                          Unix.kill (Unix.getppid ()) Sys.sigterm)
                   | kidpid ->
                       (* this is the fork's parent, the master's child *)
                       (try
                            while true do
                              let line = input_line remote_in in
                              Printf.fprintf local_out "%s\n%!" line
                            done
                        with End_of_file ->
                          (* kill my twin cause we're done *)
                          Unix.kill kidpid Sys.sigterm));
  
                exit 0  (* whoever's still alive bites it *)
  
            | kidpid ->
                (* remember his start time *)
                Hashtbl.replace children kidpid (Unix.time ());
                Unix.close remote_server;  (* no use to master *)
                Unix.close local_client;   (* likewise *)
        end
      with Unix.Unix_error (Unix.EINTR, "accept", _) -> ()
    done
  
  let () =
    check_args ();                (* processing switches *)
    start_proxy ();               (* launch our own server *)
    service_clients ();           (* wait for incoming *)
    prerr_endline "NOT REACHED";  (* you can't get here from there *)
    exit 1
  
  

