(* ********************************************************************** *)
(* Using Whois to Retrieve Information from the InterNIC *)
(* ********************************************************************** *)
let pleac_Using_Whois_to_Retrieve_Information_from_the_InterNIC () = 
  (* WHOIS servers depend on the TLD, and their output formats are
     informal, inconsistent, and completely different from server
     to server. This makes a general solution very large and ad-hoc.
     The Net::Whois package, on which the original Perl recipe was
     based, no longer works since WHOIS servers started redirecting
     to other servers for most of the information.
  
     Since no libraries are available for this task, we will do a
     WHOIS lookup manually using sockets. This example shows how to
     perform a WHOIS lookup for the "sourceforge.net" domain, and
     probably will not work without modification for domains under
     any other TLD. *)
  
  #load "unix.cma";;
  #load "str.cma";;
  
  let domain_name = "sourceforge.net"
  let whois_server = "whois.internic.net"
  let service = Unix.getservbyname "whois" "tcp"
  
  let ltrim =
    let re = Str.regexp "^[ \r\n\t\x00\x0B]*" in
    Str.global_replace re ""
  
  let () =
    (* Connect to the parent server to find the redirect. *)
  
    let host = Unix.gethostbyname whois_server in
    let socket_in, socket_out =
      Unix.open_connection
        (Unix.ADDR_INET (host.Unix.h_addr_list.(0),
                         service.Unix.s_port)) in
  
    output_string socket_out domain_name;
    output_string socket_out "\n";
    flush socket_out;
  
    let whois_redirect_regexp = Str.regexp "Whois Server: \\(.*\\)" in
    let whois_redirect = ref "" in
  
    begin
      try
        while true do
          let line = ltrim (input_line socket_in) in
          if Str.string_match whois_redirect_regexp line 0
          then whois_redirect := Str.matched_group 1 line
        done
      with End_of_file ->
        Unix.shutdown_connection socket_in
    end;
  
    if !whois_redirect = ""
    then failwith "Couldn't find WHOIS redirect";
  
    (* Connect to the real server and get the WHOIS data. *)
  
    let host = Unix.gethostbyname !whois_redirect in
    let socket_in, socket_out =
      Unix.open_connection
        (Unix.ADDR_INET (host.Unix.h_addr_list.(0),
                         service.Unix.s_port)) in
  
    output_string socket_out domain_name;
    output_string socket_out "\n";
    flush socket_out;
  
    let domain_name_regexp = Str.regexp "Domain name: \\(.*\\)" in
    let domain_name = ref "" in
  
    let registrant_regexp = Str.regexp "Registrant:" in
    let registrant_name = ref "" in
    let registrant_address = ref [] in
    let registrant_country = ref "" in
  
    let contact_regexp = Str.regexp "\\(.*\\) Contact:" in
    let contacts = ref [] in
  
    begin
      try
        while true do
          let line = ltrim (input_line socket_in) in
          if Str.string_match domain_name_regexp line 0
          then domain_name := Str.matched_group 1 line
          else if Str.string_match registrant_regexp line 0
          then
            begin
              (* Read registrant data. *)
              registrant_name := ltrim (input_line socket_in);
              let finished = ref false in
              while not !finished do
                let line = ltrim (input_line socket_in) in
                if String.length line > 2
                then registrant_address := !registrant_address @ [line]
                else if String.length line = 2
                then registrant_country := line
                else finished := true
              done
            end
          else if Str.string_match contact_regexp line 0
          then
            begin
              (* Read contact data. *)
              let contact_type = Str.matched_group 1 line in
              let contact_info = ref [] in
              for i = 1 to 6 do
                let line = ltrim (input_line socket_in) in
                contact_info := !contact_info @ [line]
              done;
              contacts := (contact_type, !contact_info) :: !contacts
            end
        done
      with End_of_file ->
        Unix.shutdown_connection socket_in
    end;
  
    (* Display the results. *)
  
    Printf.printf "The domain is called %s\n" !domain_name;
    Printf.printf "Mail for %s should be sent to:\n" !registrant_name;
    List.iter (Printf.printf "\t%s\n") !registrant_address;
    Printf.printf "\t%s\n" !registrant_country;
  
    if !contacts = []
    then Printf.printf "No contact information.\n"
    else
      begin
        Printf.printf "Contacts:\n";
        List.iter
          (fun (contact_type, contact_info) ->
             Printf.printf "  %s\n" contact_type;
             List.iter (Printf.printf "    %s\n") contact_info)
          !contacts
      end
  

