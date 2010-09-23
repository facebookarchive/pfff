(* ********************************************************************** *)
(* Being an FTP Client *)
(* ********************************************************************** *)
let pleac_Being_an_FTP_Client () = 
  (* The Netclient package from Ocamlnet provides an event-driven
     FTP client. This client does not currently support uploading.
  
     Ocamlnet is available here:
     http://projects.camlcity.org/projects/ocamlnet.html
  
     This recipe assumes it has been installed with findlib. *)
  
  #use "topfind";;
  #require "netclient";;
  
  (* Create an FTP client instance. *)
  let ftp = new Ftp_client.ftp_client ()
  
  (* Build and execute a chain of FTP methods. *)
  let () =
    ftp#add (new Ftp_client.connect_method ~host:"127.0.0.1" ());
    ftp#add (new Ftp_client.login_method
               ~user:"anonymous"
               ~get_password:(fun () -> "user@example.com")
               ~get_account:(fun () -> "anonymous") ());
    ftp#add (new Ftp_client.walk_method (`Dir "/pub"));
    let ch = new Netchannels.output_channel (open_out "output.txt") in
    ftp#add (new Ftp_client.get_method
               ~file:(`Verbatim "index.txt")
               ~representation:`Image
               ~store:(fun _ -> `File_structure ch) ());
    ftp#run ()
  
  (*-----------------------------*)
  
  (* If an error occurs, it will be exposed by the "state" property. *)
  let () =
    match ftp#state with
      | `Error (Ftp_client.FTP_error (Unix.Unix_error (e, _, _))) ->
          Printf.eprintf "Error: %s\n%!"
            (Unix.error_message e)
      | _ -> ()
  
  (*-----------------------------*)
  
  (* To determine the current working directory, send invoke the `PWD
     command and inspect the result in a callback. *)
  let () =
    ftp#add (new Ftp_client.invoke_method
               ~command:`PWD
               ~process_result:(fun state (code, message) ->
                                  Printf.printf
                                    "I'm in the directory %s\n%!"
                                    message) ())
  
  (*-----------------------------*)
  
  (* Use mkdir_method and rmdir_method to make and remove directories from
     the remote server. Use the optional ~onerror argument to specify an
     error handler. *)
  let () =
    ftp#add
      ~onerror:(fun e ->
                  Printf.eprintf "Can't create /ocaml: %s\n%!"
                    (Printexc.to_string e))
      (new Ftp_client.mkdir_method (`Verbatim "/pub/ocaml"))
  
  (*-----------------------------*)
  
  (* Use a list_method to get a list of files in a remote directory. *)
  let () =
    let buffer = Buffer.create 256 in
    let ch = new Netchannels.output_buffer buffer in
    ftp#add
      ~onsuccess:(fun () -> print_endline (Buffer.contents buffer))
      ~onerror:(fun e ->
                  Printf.eprintf "Can't get a list of files in /pub: %s\n%!"
                    (Printexc.to_string e))
      (new Ftp_client.list_method
         ~dir:(`Verbatim "/pub")
         ~representation:`Image
         ~store:(fun _ -> `File_structure ch) ())
  
  (*-----------------------------*)
  
  (* Use `QUIT followed by ftp#abort to close the connection and exit
     the event loop. *)
  let () =
    ftp#add (new Ftp_client.invoke_method
               ~command:`QUIT
               ~process_result:(fun _ _ -> ftp#abort ()) ())
  

