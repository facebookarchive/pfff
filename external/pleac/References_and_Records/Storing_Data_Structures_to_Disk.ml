(* ********************************************************************** *)
(* Storing Data Structures to Disk *)
(* ********************************************************************** *)
let pleac_Storing_Data_Structures_to_Disk () = 
  let () =
    (* Store a data structure to disk. *)
    let out_channel = open_out_bin "filename" in
    Marshal.to_channel out_channel data [];
    close_out out_channel;
  
    (* Load a data structure from disk. *)
    let in_channel = open_in_bin "filename" in
    let data = Marshal.from_channel in_channel in
    (* ... *)
    ();;
  
  #load "unix.cma";;
  let () =
    (* Store a data structure to disk, with exclusive locking. *)
    let out_channel = open_out_bin "filename" in
    Unix.lockf (Unix.descr_of_out_channel out_channel) Unix.F_LOCK 0;
    Marshal.to_channel out_channel data [];
    close_out out_channel;
  
    (* Load a data structure from disk, with shared locking. *)
    let in_channel = open_in_bin "filename" in
    Unix.lockf (Unix.descr_of_in_channel in_channel) Unix.F_RLOCK 0;
    let data = Marshal.from_channel in_channel in
    (* ... *)
    ()
  

