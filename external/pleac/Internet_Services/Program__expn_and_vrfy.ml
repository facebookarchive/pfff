(* ********************************************************************** *)
(* Program: expn and vrfy *)
(* ********************************************************************** *)
let pleac_Program__expn_and_vrfy () = 
  #!/usr/bin/ocaml
  (* expn -- convince smtp to divulge an alias expansion *)
  
  #use "topfind";;                        (* Findlib *)
  #require "str";;                        (* Stdlib *)
  #require "unix";;                       (* Stdlib *)
  #require "perl";;                       (* Perl4caml *)
  #require "smtp";;                       (* Ocamlnet *)
  let _ = Perl.eval "use Net::DNS"        (* Net::DNS *)
  
  let selfname = Unix.gethostname ()
  
  let () =
    if Array.length Sys.argv < 2
    then (Printf.eprintf "usage: %s address@host ...\n" Sys.argv.(0);
          exit 1)
  
  let () =
    List.iter
      (fun combo ->
         let name, host =
           match Str.bounded_split (Str.regexp "@") combo 2 with
             | [] -> "", ""
             | [name] -> name, "localhost"
             | [name; host] -> name, host
             | _ -> assert false in
         let hosts =
           Perl.call_array ~fn:"mx" [Perl.sv_of_string host] in
         let hosts =
           List.map (fun mx -> Perl.call_method mx "exchange" []) hosts in
         let hosts =
           if hosts = [] then [Perl.sv_of_string host] else hosts in
         List.iter
           (fun host ->
              let host = Perl.string_of_sv host in
              Printf.printf "Expanding %s at %s (%s): %!"
                name host combo;
              let inet_addr =
                (Unix.gethostbyname host).Unix.h_addr_list.(0) in
              let addr = Unix.ADDR_INET (inet_addr, Netsmtp.tcp_port) in
              try
                let ic, oc = Unix.open_connection addr in
                let smtp =
                  new Netsmtp.client
                    (new Netchannels.input_channel ic)
                    (new Netchannels.output_channel oc) in
                ignore (smtp#helo ~host:selfname ());
                print_endline
                  (match smtp#expn name with
                     | None -> "None"
                     | Some results -> String.concat ", " results);
                smtp#quit ();
                Unix.shutdown_connection ic;
                close_out oc
              with Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
                Printf.eprintf "cannot connect to %s\n" host)
           hosts)
      (List.tl (Array.to_list Sys.argv))
  
  

