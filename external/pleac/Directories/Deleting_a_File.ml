(* ********************************************************************** *)
(* Deleting a File *)
(* ********************************************************************** *)
let pleac_Deleting_a_File () = 
  unlink filename;;			(* use unix library *)
  Sys.remove filename;;			(* in the standard library *)
  
  let error_flag = ref(None) in
  let local_unlink filename =
    try
      unlink filename
    with
      | Unix_error (er,_,_) -> 
  	error_flag := (Some er) in
  List.iter local_unlink filenames;
  match !error_flag with
    | Some er ->
        Printf.eprintf "Couldn't unlink all of";
        List.iter (Printf.eprintf " %s") filenames;
        Printf.eprintf ": %s\n" (error_message er)
    | None ();;
  
  
  (*-----------------------------*)
  
  let error_flag = ref(0) in
  let local_unlink count filename =
    try
      unlink filename;
      count + 1
    with
      | Unix_error (er,_,_) -> 
  	count in
  let count = (List.fold_left local_unlink filenames 0) 
  and len = List.length filenames in
  if count <> len
  then
    Printf.eprintf "Could only delete %i of %i file\n" count len;;
  
  (*-----------------------------*)
  

