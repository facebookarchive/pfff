(* ********************************************************************** *)
(* Debugging the Raw HTTP Exchange *)
(* ********************************************************************** *)
let pleac_Debugging_the_Raw_HTTP_Exchange () = 
  #!/usr/bin/ocaml
  (* dummyhttpd - start an HTTP daemon and print what the client sends *)
  
  #load "unix.cma";;
  
  let host = "localhost"
  let port = 8989
  
  let () =
    Printf.printf "Please contact me at: http://%s:%d/\n%!" host port;
    let addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
    let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt server Unix.SO_REUSEADDR true;
    Unix.bind server (Unix.ADDR_INET (addr, port));
    Unix.listen server 10;
    while true do
      begin
        let client, sockaddr = Unix.accept server in
        let in_channel = Unix.in_channel_of_descr client in
        try
          while true do
            let line = input_line in_channel in
            print_endline line
          done
        with End_of_file ->
          print_endline "EOF";
          close_in in_channel
      end
    done
  

