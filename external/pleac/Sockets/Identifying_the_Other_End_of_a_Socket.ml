(* ********************************************************************** *)
(* Identifying the Other End of a Socket *)
(* ********************************************************************** *)
let pleac_Identifying_the_Other_End_of_a_Socket () = 
  #load "unix.cma";;
  
  (* Get the remote IP address. *)
  let () =
    let other_end = Unix.getpeername socket in
    let name_info = Unix.getnameinfo other_end [Unix.NI_NUMERICHOST] in
    let ip_address = name_info.Unix.ni_hostname in
    (* ... *)
    ()
  
  (*-----------------------------*)
  
  (* Attempt to determine the remote host name, with forward and reverse
     DNS lookups to detect spoofing. *)
  let () =
    let other_end = Unix.getpeername socket in
    let name_info = Unix.getnameinfo other_end [Unix.NI_NUMERICHOST] in
    let actual_ip = name_info.Unix.ni_hostname in
    let claimed_hostname =
      (Unix.gethostbyaddr (Unix.inet_addr_of_string actual_ip))
        .Unix.h_name in
    let name_lookup = Unix.gethostbyname claimed_hostname in
    let resolved_ips =
      Array.to_list (Array.map
                       Unix.string_of_inet_addr
                       name_lookup.Unix.h_addr_list) in
    (* ... *)
    ()
  

