(* ********************************************************************** *)
(* Reading Mail with POP3 *)
(* ********************************************************************** *)
let pleac_Reading_Mail_with_POP3 () = 
  (* Use Netpop, which is part of Ocamlnet. *)
  #use "topfind";;
  #require "pop";;
  
  (* To create a Netpop client, you need to look up the server address
     and build a network connection first. Netpop uses wrappers called
     Netchannels to abstract the input and output channels. *)
  let inet_addr =
    (Unix.gethostbyname mail_server).Unix.h_addr_list.(0)
  let addr = Unix.ADDR_INET (inet_addr, Netpop.tcp_port)
  let ic, oc = Unix.open_connection addr
  let pop =
    new Netpop.client
      (new Netchannels.input_channel ic)
      (new Netchannels.output_channel oc)
  let () =
    pop#user username;
    pop#pass password
  
  (* Messages are retreived as a hashtable from message IDs to tuples,
     each tuple containing the message size in bytes and a string of
     server-specific extension data. *)
  let messages = pop#list ()
  let () =
    Hashtbl.iter
      (fun msgid (size, ext) ->
         let message = pop#retr msgid in
         (* message is a Netchannels.in_obj_channel *)
         pop#dele msgid)
      messages
  
  (*-----------------------------*)
  
  (* Use pop#apop instead of pop#user/pop#pass to avoid sending passwords
     in plaintext across the network. *)
  let () = pop#apop username password
  
  (*-----------------------------*)
  
  (* Get a message by number and print it to the console. *)
  let () =
    Printf.printf "Retrieving %d : %!" msgnum;
    try
      let message = pop#retr msgnum in
      print_newline ();
      print_endline
        (Netchannels.string_of_in_obj_channel message)
    with Netpop.Err_status e ->
      Printf.printf "failed (%s)\n%!" e
  
  (*-----------------------------*)
  
  (* Gracefully tear down the connection. *)
  let () =
    pop#quit ();
    Unix.shutdown_connection ic;
    close_out oc
  

