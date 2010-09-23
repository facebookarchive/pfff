(* ********************************************************************** *)
(* Non-Forking Servers *)
(* ********************************************************************** *)
let pleac_Non_Forking_Servers () = 
  #!/usr/bin/ocaml
  (* nonforker - server who multiplexes without forking *)
  #load "unix.cma";;
  
  let port = 1685                         (* change this at will *)
  
  (* Listen to port. *)
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
  let () =
    Unix.setsockopt server Unix.SO_REUSEADDR true;
    Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen server 10;
    Unix.set_nonblock server
  
  module FDSet =
    Set.Make(struct type t = Unix.file_descr let compare = compare end)
  let clients = ref (FDSet.singleton server)
  
  (* begin with empty buffers *)
  let inbuffer = Hashtbl.create 0
  let outbuffer = Hashtbl.create 0
  let ready = Hashtbl.create 0
  
  let buffer_size = 8192
  let buffer = String.make buffer_size '\000'
  
  (* handle deals with all pending requests for client *)
  let handle client requests =
    (* requests are in ready[client] *)
    (* send output to outbuffer[client] *)
    List.iter
      (fun request ->
         (* request is the text of the request *)
         let data = Printf.sprintf "You said: %s\n" request in
         (* put text of reply into outbuffer[client] *)
         Hashtbl.replace outbuffer client
           (try Hashtbl.find outbuffer client ^ data
            with Not_found -> data))
      requests
  
  (* Main loop: check reads/accepts, check writes, check ready to process *)
  let () =
    while true do
      (* check for new information on the connections we have *)
  
      let (can_read, _, _) =
        Unix.select (FDSet.elements !clients) [] [] 1.0 in
      List.iter
        (fun client ->
           if client = server
           then
             begin
               (* accept a new connection *)
               let (client, addr) = Unix.accept server in
               clients := FDSet.add client !clients;
               Unix.set_nonblock client
             end
           else
             begin
               (* read data *)
               let chars_read =
                 try
                   Some (Unix.read client buffer 0 buffer_size)
                 with Unix.Unix_error (error, _, _) ->
                   prerr_endline (Unix.error_message error);
                   None in
  
               match chars_read with
                 | None | Some 0 ->
                     (* This would be the end of file, so close the client *)
                     Hashtbl.remove inbuffer client;
                     Hashtbl.remove outbuffer client;
                     Hashtbl.remove ready client;
                     
                     clients := FDSet.remove client !clients;
                     Unix.close client
  
                 | Some chars_read ->
                     let data = String.sub buffer 0 chars_read in
                     Hashtbl.replace inbuffer client
                       (try Hashtbl.find inbuffer client ^ data
                        with Not_found -> data);
  
                     (* test whether the data in the buffer or the data we *)
                     (* just read means there is a complete request waiting *)
                     (* to be fulfilled.  If there is, set ready[client] *)
                     (* to the requests waiting to be fulfilled. *)
                     try
                       while true do
                         let data = Hashtbl.find inbuffer client in
                         let index = String.index data '\n' in
                         Hashtbl.replace inbuffer client
                           (String.sub data
                              (index + 1)
                              (String.length data - index - 1));
                         Hashtbl.replace ready client
                           ((try Hashtbl.find ready client
                             with Not_found -> [])
                            @ [String.sub data 0 index])
                       done
                     with Not_found -> ()
             end)
        can_read;
  
      (* Any complete requests to process? *)
      Hashtbl.iter handle ready;
      Hashtbl.clear ready;
  
      (* Buffers to flush? *)
      let (_, can_write, _) =
        Unix.select [] (FDSet.elements !clients) [] 1.0 in
      (* Skip client if we have nothing to say *)
      let can_write =
        List.filter (Hashtbl.mem outbuffer) can_write in
      List.iter
        (fun client ->
           let data = Hashtbl.find outbuffer client in
           let chars_written =
             try
               Some (Unix.single_write client data 0 (String.length data))
             with
               | Unix.Unix_error (Unix.EAGAIN, _, _)
               | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
                   prerr_endline "I was told I could write, but I can't.";
                   Some 0
               | Unix.Unix_error (error, _, _) ->
                   prerr_endline (Unix.error_message error);
                   None in
  
           match chars_written with
             | Some chars_written ->
                 if chars_written = String.length data
                 then Hashtbl.remove outbuffer client
                 else Hashtbl.replace outbuffer client
                   (String.sub data chars_written
                      (String.length data - chars_written))
             | None ->
                 (* Couldn't write all the data, and it wasn't because *)
                 (* it would have blocked.  Shutdown and move on. *)
                 Hashtbl.remove inbuffer client;
                 Hashtbl.remove outbuffer client;
                 Hashtbl.remove ready client;
                 
                 clients := FDSet.remove client !clients;
                 Unix.close client)
        can_write;
  
      let (_, _, has_exception) =
        Unix.select [] [] (FDSet.elements !clients) 0.0 in
      List.iter
        (fun client ->
           (* Deal with out-of-band data here, if you want to. *)
           ())
        has_exception;
    done
  

