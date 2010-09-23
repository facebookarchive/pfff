(* ********************************************************************** *)
(* Getting and Setting Timestamps *)
(* ********************************************************************** *)
let pleac_Getting_and_Setting_Timestamps () = 
  let (readtime, writetime) =
    let inode = stat filename in
    (inode.st_atime, inode.st_mtime);;
  
  utimes filename newreadtime newwritetime;;
  
  (*-----------------------------*)
  
  let second_per_day = 60. *. 60. *. 24. in
  let (atime, mtime) =
    let inode = stat filename in
    (inode.st_atime, inode.st_mtime) in
  let newreadtime = atime -. 7. *. second_per_day
  and newwritetime = mtime -. 7. *. second_per_day in
  try 
    utimes filename newreadtime newwritetime 
  with
    | Unix_error (er,_,_) ->
        Printf.eprintf 
  	"couldn't backdate %s by a week w/ utime: %s\n"
  	filename (error_message er);;
  
  (*-----------------------------*)
  let mtime = (stat file).st_mtime in
  utimes file (time ()) mtime  ;;
  
  (*-----------------------------*)
  
  (* compile with ocamlc unix.cma uvi.ml -o uvi *)
  open Unix
  
  let main () =
    if (Array.length Sys.argv <> 2)
    then
      Printf.eprintf "Usage: uvi filename\n";
    let filename = Sys.argv.(1) in
    let atime,mtime = 
      let st = stat filename in
      (st.st_atime, st.st_mtime) in
    let editor =
      begin
        try
  	Sys.getenv "editor"
        with
  	| Not_found -> "vi"
      end in
    Sys.command (Printf.sprintf "%s %s" editor filename);
    utimes filename atime mtime in
  main ();;
    
  (*-----------------------------*)
  

