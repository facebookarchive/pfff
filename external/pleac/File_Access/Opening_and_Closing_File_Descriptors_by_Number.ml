(* ********************************************************************** *)
(* Opening and Closing File Descriptors by Number *)
(* ********************************************************************** *)
let pleac_Opening_and_Closing_File_Descriptors_by_Number () = 
  (* An abstraction barrier exists between file descriptor numbers and
     file_descr values, but Ocamlnet provides functions in the Netsys
     module to map between the two. *)
  #load "unix.cma";;
  #directory "+netsys";;
  #load "netsys.cma";;
  
  (* Open the descriptor itself. *)
  let file_descr = Netsys.file_descr_of_int fdnum
  let in_channel = Unix.in_channel_of_descr file_descr
  
  (* Open a copy of the descriptor. *)
  let file_descr = Unix.dup (Netsys.file_descr_of_int fdnum)
  let in_channel = Unix.in_channel_of_descr file_descr
  
  (* After processing... *)
  let () = close_in in_channel
  

