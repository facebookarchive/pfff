(* ********************************************************************** *)
(* Sending Mail *)
(* ********************************************************************** *)
let pleac_Sending_Mail () = 
  (* Use Netsendmail, part of the Netstring package that comes with
     Ocamlnet, to send mail through a command-line mailer program. *)
  
  #use "topfind";;
  #require "netstring";;
  
  let () =
    Netsendmail.sendmail
      ~mailer:"/usr/sbin/sendmail"  (* defaults to "/usr/lib/sendmail" *)
      (Netsendmail.compose
         ~from_addr:(from_name, from_address)
         ~to_addrs:[(to_name, to_address)]
         ~subject:subject
         body)
  
  (*-----------------------------*)
  
  (* You can also open a pipe directly to sendmail. *)
  
  #load "unix.cma";;
  
  let () =
    let sendmail =
      Unix.open_process_out "/usr/lib/sendmail -oi -t -odq" in
    output_string sendmail "\
  From: User Originating Mail <me@host>
  To: Final Destination <you@otherhost>
  Subject: A relevant subject line
  
  Body of the message goes here, in as many lines as you like.
  ";
    ignore (Unix.close_process_out sendmail)
  

