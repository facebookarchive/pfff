(* ********************************************************************** *)
(* Finding Your Own Name and Address *)
(* ********************************************************************** *)
let pleac_Finding_Your_Own_Name_and_Address () = 
  (*-----------------------------*)
  
  (*
  ** Finding Your Own Name and Address.
  ** The Unix module to the rescue again.
  *)
  
  #load "unix.cma" ;;
  open Unix ;;
  
  let hostname = gethostname () in
  Printf.printf "hostname : %s\n" hostname ;;
  
  (*-----------------------------*)
  
  (*
  ** Unfortunately there is no easy way of retreiving the
  ** uname without using Unix.open_process_in.
  *)
  
  (*-----------------------------*)
  
  let hentry = gethostbyname hostname in
  let address = hentry.h_addr_list.(0) in
  Printf.printf "address : %s\n" (string_of_inet_addr address) ;;
  
  let hentry = gethostbyaddr address in
  Printf.printf "hostname : %s\n" hentry.h_name ;;
  
  

