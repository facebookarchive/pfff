(* ********************************************************************** *)
(* Communicating over TCP *)
(* ********************************************************************** *)
let pleac_Communicating_over_TCP () = 
  #load "unix.cma";;
  
  let () =
    let server_in = Unix.in_channel_of_descr server in
    let server_out = Unix.out_channel_of_descr server in
    output_string server_out "What is your name?\n";
    flush server_out;
    let response = input_line server_in in
    print_endline response
  
  (*-----------------------------*)
  
  let () =
    try
      ignore
        (Unix.send server data_to_send 0 (String.length data_to_send) flags)
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "Can't send: %s\n%!"
        (Unix.error_message e);
      exit 1
  
  let data_read =
    let data_read = String.create maxlen in
    let data_length =
      try
        Unix.recv server data_read 0 maxlen flags
      with Unix.Unix_error (e, _, _) ->
        Printf.eprintf "Can't receive: %s\n%!"
          (Unix.error_message e);
        exit 1 in
    String.sub data_read 0 data_length
  
  (*-----------------------------*)
  
  let () =
    let read_from, _, _ =
      Unix.select [from_server; to_client] [] [] timeout in
    List.iter
      (fun socket ->
         (* read the pending data from socket *)
         ())
      read_from
  
  (*-----------------------------*)
  
  (* Requires OCaml 3.11 or newer. *)
  let () =
    try Unix.setsockopt server Unix.TCP_NODELAY true
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "Couldn't disable Nagle's algorithm: %s\n%!"
        (Unix.error_message e)
  
  (*-----------------------------*)
  
  (* Requires OCaml 3.11 or newer. *)
  let () =
    try Unix.setsockopt server Unix.TCP_NODELAY false
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "Couldn't enable Nagle's algorithm: %s\n%!"
        (Unix.error_message e)
  

