(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  open Unix 
  
  (* handle_unix_error generates a nice error message and exits *)
  let entry = handle_unix_error stat "/usr/bin/vi"
  let entry = handle_unix_error stat "/usr/bin/"
  let entry = handle_unix_error fstat filedescr
  
  (* without handle_unix_error an exception is raised for errors *)
  let inode = stat "/usr/bin/vi"
  let ctime = inode.st_ctime
  let size = inode.st_size
  
  (* don't know any equivalent in ocaml *)
  (* maybe one could use file(1) (to know if it is an ASCII text file) *)
  let dirhandle = handle_unix_error opendir "/usr/bin" in
  begin
    try
      while true do
        let file = readdir dirhandle in
        Printf.printf "Inside /usr/bin is something called %s\n" file
      done
    with
      | End_of_file -> ()
  end;
  closedir dirhandle;;
  

