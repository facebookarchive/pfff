(* ********************************************************************** *)
(* Writing Bidirectional Clients *)
(* ********************************************************************** *)
let pleac_Writing_Bidirectional_Clients () = 
  #!/usr/bin/ocaml
  (* biclient - bidirectional forking client *)
  #load "unix.cma";;
  
  let host, port =
    match Array.to_list Sys.argv with
      | [_; host; port] -> host, int_of_string port
      | _ -> Printf.eprintf "usage: %s host port\n" Sys.argv.(0); exit 1
  
  let sockaddr =
    let addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
    Unix.ADDR_INET (addr, port)
  
  let () =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect socket sockaddr;
    Printf.eprintf "[Connected to %s:%d]\n%!" host port;
  
    (* split the program into two processes, identical twins *)
    match Unix.fork () with
      | 0 ->
          (* child copies standard input to the socket *)
          let output = Unix.out_channel_of_descr socket in
          while true do
            let line = input_line stdin in
            output_string output line;
            output_string output "\n";
            flush output
          done
      | kidpid ->
          (* parent copies the socket to standard output *)
          let input = Unix.in_channel_of_descr socket in
          try
            while true do
              let line = input_line input in
              output_string stdout line;
              output_string stdout "\n";
              flush stdout
            done
          with End_of_file ->
            Unix.kill kidpid Sys.sigterm
  
  let () = exit 0
  

