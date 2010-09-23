(* ********************************************************************** *)
(* Processing All Files in a Directory *)
(* ********************************************************************** *)
let pleac_Processing_All_Files_in_a_Directory () = 
  (* Using Sys.readdir. *)
  let () =
    Array.iter
      (fun file ->
         let path = Filename.concat dirname file in
         (* do something with path *)
         ())
      (Sys.readdir dirname)
  
  (*-----------------------------*)
  
  (* Using Unix.opendir, readdir, and closedir. Note that the "." and ".."
     directories are included in the result unlike with Sys.readdir. *)
  #load "unix.cma";;
  
  let () =
    let dir =
      try Unix.opendir dirname
      with Unix.Unix_error (e, _, _) ->
        Printf.eprintf "can't opendir %s: %s\n"
          dirname (Unix.error_message e);
        exit 255 in
    try
      while true do
        let file = Unix.readdir dir in
        let path = Filename.concat dirname file in
        (* do something with path *)
        ()
      done
    with End_of_file ->
      Unix.closedir dir
  
  (*-----------------------------*)
  
  (* Get a list of full paths to plain files. *)
  let plainfiles dir =
    List.filter
      (fun path ->
         match Unix.lstat path with
           | {Unix.st_kind=Unix.S_REG} -> true
           | _ -> false)
      (List.map
         (Filename.concat dir)
         (Array.to_list (Sys.readdir dir)))
  

