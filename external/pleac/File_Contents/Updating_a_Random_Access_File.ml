(* ********************************************************************** *)
(* Updating a Random-Access File *)
(* ********************************************************************** *)
let pleac_Updating_a_Random_Access_File () = 
  let () =
    let address = recsize * recno in
    seek_in in_channel address;
    let buffer = String.create recsize in
    really_input in_channel buffer 0 recsize;
    close_in in_channel;
    (* update fields, then *)
    seek_out out_channel address;
    output_string out_channel buffer;
    close_out out_channel
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* weekearly -- set someone's login date back a week *)
  #load "unix.cma";;
  
  let sizeof = 4 + 12 + 16
  let user =
    if Array.length Sys.argv > 1
    then Sys.argv.(1)
    else (try Sys.getenv "USER"
          with Not_found -> Sys.getenv "LOGNAME")
  
  let address = (Unix.getpwnam user).Unix.pw_uid * sizeof
  
  let () =
    let lastlog = open_in "/var/log/lastlog" in
    seek_in lastlog address;
    let line = String.make 12 ' ' in
    let host = String.make 16 ' ' in
    let time = input_binary_int lastlog in
    really_input lastlog line 0 12;
    really_input lastlog host 0 16;
    let buffer = String.create sizeof in
    really_input lastlog buffer 0 sizeof;
    close_in lastlog;
  
    let time = time - 24 * 7 * 60 * 60 in (* back-date a week *)
  
    let lastlog = open_out_gen [Open_wronly] 0o666 "/var/log/lastlog" in
    seek_out lastlog address;
    output_binary_int lastlog time;
    close_out lastlog
  

