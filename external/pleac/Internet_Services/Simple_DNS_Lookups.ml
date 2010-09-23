(* ********************************************************************** *)
(* Simple DNS Lookups *)
(* ********************************************************************** *)
let pleac_Simple_DNS_Lookups () = 
  #load "unix.cma";;
  
  let () =
    try
      let addresses = Unix.gethostbyname name in
      let addresses =
        Array.map Unix.string_of_inet_addr addresses.Unix.h_addr_list in
      (* addresses is an array of IP addresses *)
      Array.iter print_endline addresses
    with Not_found ->
      Printf.printf "Can't resolve %s\n" name
  
  (*-----------------------------*)
  
  let () =
    try
      let host = Unix.gethostbyaddr (Unix.inet_addr_of_string address) in
      let name = host.Unix.h_name in
      (* name is the hostname ("www.perl.com") *)
      print_endline name
    with Not_found ->
      Printf.printf "Can't resolve %s\n" address
  
  (*-----------------------------*)
  
  let () =
    try
      let host = Unix.gethostbyaddr (Unix.inet_addr_of_string address) in
      let name = host.Unix.h_name in
      try
        let addresses = Unix.gethostbyname name in
        let addresses =
          Array.map Unix.string_of_inet_addr addresses.Unix.h_addr_list in
        Array.iter print_endline addresses;
        let found = List.mem address (Array.to_list addresses) in
        print_endline (if found then "found" else "not found")
      with Not_found ->
        Printf.printf "Can't look up %s\n" name
    with Not_found ->
      Printf.printf "Can't look up %s\n" address
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* mxhost - find mx exchangers for a host *)
  
  (* Though there is an experimental new DNS resolver for OCaml called
     Netdns, it does not yet support resolving MX records. For now, we'll
     use Net::DNS through perl4caml until a better solution is available.
  *)
  #directory "+perl";;
  #load "perl4caml.cma";;
  let _ = Perl.eval "use Net::DNS"
  
  let host = Sys.argv.(1)
  let res = Perl.call_class_method "Net::DNS::Resolver" "new" []
  let mx = Perl.call_array ~fn:"mx" [res; Perl.sv_of_string host]
  let () =
    if mx = [] then
      Printf.eprintf "Can't find MX records for %s (%s)\n"
        host (Perl.string_of_sv (Perl.call_method res "errorstring" []))
  
  let () =
    List.iter
      (fun record ->
         let preference = Perl.call_method record "preference" [] in
         let exchange = Perl.call_method record "exchange" [] in
         Printf.printf "%s %s\n"
           (Perl.string_of_sv preference)
           (Perl.string_of_sv exchange))
      mx
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* hostaddrs - canonize name and show addresses *)
  #load "unix.cma";;
  let name = Sys.argv.(1)
  let hent = Unix.gethostbyname name
  let () =
    Printf.printf "%s => %s\n"
      hent.Unix.h_name    (* in case different *)
      (String.concat " "
         (Array.to_list
            (Array.map
               Unix.string_of_inet_addr
               hent.Unix.h_addr_list)))
  

