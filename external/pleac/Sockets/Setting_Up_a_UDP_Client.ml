(* ********************************************************************** *)
(* Setting Up a UDP Client *)
(* ********************************************************************** *)
let pleac_Setting_Up_a_UDP_Client () = 
  #load "unix.cma";;
  
  (* Create a UDP socket. *)
  let socket =
    Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  
  (*-----------------------------*)
  
  (* Send a UDP message. *)
  let ipaddr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0)
  let portaddr = Unix.ADDR_INET (ipaddr, portno)
  let len = Unix.sendto socket msg 0 (String.length msg) [] portaddr
  
  (*-----------------------------*)
  
  (* Receive a UDP message. *)
  let msg = String.create maxlen
  let len, portaddr = Unix.recvfrom socket msg 0 maxlen []
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* clockdrift - compare another system's clock with this one *)
  #load "unix.cma";;
  
  let secs_of_70_years = 2_208_988_800L
  
  let msgbox =
    Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  
  let him =
    Unix.ADDR_INET ((Unix.gethostbyname
                       (if Array.length Sys.argv > 1
                        then Sys.argv.(1)
                        else "127.1")).Unix.h_addr_list.(0),
                    (Unix.getservbyname "time" "udp").Unix.s_port)
  
  let () = ignore (Unix.sendto msgbox "" 0 0 [] him)
  
  let ptime = String.create 4
  
  let host =
    match Unix.recvfrom msgbox ptime 0 4 [] with
      | _, Unix.ADDR_INET (addr, port) ->
          (Unix.gethostbyaddr addr).Unix.h_name
      | _ -> assert false
  
  let delta =
    Int64.to_float
      (Int64.sub
         (Int64.of_string (Printf.sprintf "0x%02x%02x%02x%02x"
                             (int_of_char ptime.[0])
                             (int_of_char ptime.[1])
                             (int_of_char ptime.[2])
                             (int_of_char ptime.[3])))
         secs_of_70_years)
      -. (Unix.time ())
  
  let () =
    Printf.printf "Clock on %s is %d seconds ahead of this one.\n"
      host (int_of_float delta)
  

