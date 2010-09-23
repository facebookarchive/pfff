(* ********************************************************************** *)
(* Simulating Telnet from a Program *)
(* ********************************************************************** *)
let pleac_Simulating_Telnet_from_a_Program () = 
  (* To simulate a Telnet client with OCaml, you can use the
     Telnet_client module from Ocamlnet's "netclient" package.
  
     This module is written in an asynchronous style, so you
     will need to create event handlers to process the Telnet
     events that occur: data, end of file, timeout, and the
     sending and receiving of options (also known as "do",
     "don't", "will", and "won't". *)
  
  #use "topfind";;
  #require "netclient";;
  
  open Telnet_client
  
  (* This class wraps the Telnet session for convenience in
     defining event handlers and chaining them together. *)
  class session ~host ~port ~username ~password ~prompt ~timeout =
  object (self)
    (* Telnet_client.telnet_session instance to wrap. *)
    val telnet = new telnet_session
  
    (* Initial on-data handler, which will be redefined later. *)
    val mutable process = fun _ -> ()
  
    (* Initialize the Telnet session. *)
    initializer
      telnet#set_connection (Telnet_connect (host, port));
      telnet#set_options {connection_timeout=timeout;
                          verbose_connection=false;
                          verbose_input=false;
                          verbose_output=false};
      telnet#set_callback self#on_input;
      telnet#set_exception_handler self#on_exception;
      telnet#attach ();
      process <- self#start
  
    (* Build an input callback that checks for a regular
       expression match in the input and calls a callback
       function if the match is positive. *)
    method waitfor pat cb =
      let rex = Pcre.regexp pat in
      fun data -> if Pcre.pmatch ~rex data then cb data
  
    (* Enqueue a line of data and flush the output queue. *)
    method write data =
      Queue.add (Telnet_data data) telnet#output_queue;
      Queue.add (Telnet_data "\n") telnet#output_queue;
      telnet#update ()
  
    (* Handle first input: wait for a login prompt and then
       invoke self#send_username to send the username. *)
    method start =
      self#waitfor "ogin:" self#send_username
  
    (* Send the username and wait for the password prompt. *)
    method send_username data =
      self#write username;
      process <- self#waitfor "assword:" self#send_password
  
    (* Send the password and wait to see if we succeeded. *)
    method send_password data =
      self#write password;
      process <- self#verify_login
  
    (* Determine if the login was a success or a failure.
       Abort with an exception on failure; call self#logged_in
       on success. *)
    method verify_login data =
      if Pcre.pmatch ~pat:"incorrect" data
      then failwith "Login failed"
      else if Pcre.pmatch ~pat:"^\\s*$" data
      then () (* ignore blank lines *)
      else self#logged_in data
  
    (* Logged in successfully. Wait for a prompt if necessary
       and call self#run_ls to send the first command. *)
    method logged_in data =
      process <- self#waitfor prompt self#run_ls;
      self#waitfor prompt self#run_ls data
  
    (* Do a directory listing and wait for results. *)
    method run_ls data =
      self#write "/bin/ls -1";
      process <- self#gather_files
  
    (* This variable will buffer the results of the "ls" command. *)
    val mutable files = ""
  
    (* Buffer the filenames printed out from the "ls" command and
       print them out once we get a prompt. *)
    method gather_files data =
      if Pcre.pmatch ~pat:prompt data
      then
        begin
          files <- Pcre.replace ~pat:"^/bin/ls -1\\s*" files;
          Printf.printf
            "Files: %s\n%!"
            (String.concat ", "
               (Pcre.split ~pat:"\\s+" files));
          self#run_top data
        end
      else files <- files ^ data
  
    (* Run another command until we get a prompt and then call
       self#close to close the connection. *)
    method run_top data =
      self#write "top -n1 -b";
      process <- self#waitfor prompt self#close
  
    (* Close the connection by sending an EOF. *)
    method close data =
      Queue.add Telnet_eof telnet#output_queue
  
    (* When we receive an EOF, exit the program. *)
    method on_eof () =
      prerr_endline "EOF";
      exit 0
  
    (* If a timeout event is received, exit with an error code. *)
    method on_timeout () =
      prerr_endline "Timeout";
      exit 1
  
    (* Print any thrown exceptions to standard error. *)
    method on_exception exn =
      prerr_endline (Printexc.to_string exn)
  
    (* This is the main error handler, which dispatches on
       Telnet_client events. *)
    method on_input got_synch =
      while not (Queue.is_empty telnet#input_queue) do
        let tc = Queue.take telnet#input_queue in
        match tc with
          | Telnet_data data -> process data
  	    | Telnet_eof -> self#on_eof ()
  	    | Telnet_timeout -> self#on_timeout ()
          | Telnet_will _
          | Telnet_wont _
          | Telnet_do _
          | Telnet_dont _ ->
              (* The telnet_session handles these events.
                 Calling this method is necessary. *)
              telnet#process_option_command tc
  	    | _ -> ()
      done
  
    (* Run the Telnet session by calling the "run" method on
       the underling telnet_session instance. *)
    method run = telnet#run
  end
  
  (* Create an instance of our custom session class. *)
  let session =
    new session
      ~host:"localhost"
      ~port:23
      ~username:"test"
      ~password:"pleac"
      ~prompt:"\\$ $"
      ~timeout:10.
  
  (* Start the session. *)
  let () = session#run ()
  

