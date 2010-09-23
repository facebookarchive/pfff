(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  open Unix
  
  (* Convert human readable form to 32 bit value *)
  let packed_ip = inet_addr_of_string "208.146.240.1" in
  
  let host = gethostbyname "www.oreilly.com" in
  let packed_ip = host.h_addr_list.(0) in
  
  (* Convert 32 bit value to ip adress *)
  let ip_address = string_of_inet_addr (packed_ip) in
  
  (* Create socket object *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  
  (* Get socketname *)
  let saddr = getsockname sock ;;
  
  

