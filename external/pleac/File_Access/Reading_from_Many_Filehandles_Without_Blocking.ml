(* ********************************************************************** *)
(* Reading from Many Filehandles Without Blocking *)
(* ********************************************************************** *)
let pleac_Reading_from_Many_Filehandles_Without_Blocking () = 
  #load "unix.cma";;
  
  let () =
    (* list all file descriptors to poll *)
    let readers = [file_descr1; file_descr2; file_descr3] in
    let ready, _, _ = Unix.select readers [] [] 0.0 in
    (* input waiting on the filehandles in "ready" *)
    ()
  
  (*-----------------------------*)
  
  let () =
    let in_channel = Unix.in_channel_of_descr file_descr in
    let found, _, _ = Unix.select [file_descr] [] [] 0.0 (* just check *) in
    match found with
      | [] -> ()
      | _ ->
          let line = input_line in_channel in
          Printf.printf "I read %s\n%!" line
  

