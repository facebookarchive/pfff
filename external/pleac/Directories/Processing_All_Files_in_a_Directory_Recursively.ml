(* ********************************************************************** *)
(* Processing All Files in a Directory Recursively *)
(* ********************************************************************** *)
let pleac_Processing_All_Files_in_a_Directory_Recursively () = 
  let rec find_files f error root =
    Array.iter
      (fun filename ->
         let path = Filename.concat root filename in
         let is_dir =
           try Some (Sys.is_directory path)
           with e -> error root e; None in
         match is_dir with
           | Some true -> if f path then find_files f error path
           | Some false -> ignore (f path)
           | None -> ())
      (try Sys.readdir root with e -> error root e; [| |])
  
  let process_file fn =
    (* Print the name of each directory and file found. *)
    Printf.printf "%s: %s\n"
      (if Sys.is_directory fn then "directory" else "file") fn;
  
    (* Prune directories named ".svn". *)
    not (Sys.is_directory fn && Filename.basename fn = ".svn")
  
  let handle_error fn exc =
    Printf.eprintf "Error reading %s: %s\n" fn (Printexc.to_string exc)
  
  let () =
    List.iter (find_files process_file handle_error) dirlist
  
  (*-----------------------------*)
  
  (* Add a trailing slash to the names of directories. *)
  let () =
    List.iter
      (find_files
         (fun fn ->
            print_endline
              (if Sys.is_directory fn then (fn ^ "/") else fn);
            true)
         (fun _ _ -> ()))
      (match List.tl (Array.to_list Sys.argv) with
         | [] -> ["."]
         | dirs -> dirs)
  
  (*-----------------------------*)
  
  (* Sum the file sizes of a directory tree. *)
  #load "unix.cma";;
  let sum = ref 0
  let () =
    List.iter
      (find_files
         (fun fn ->
            sum := !sum + (match Unix.stat fn
                           with {Unix.st_size=size} -> size);
            true)
         (fun _ _ -> ()))
      (match List.tl (Array.to_list Sys.argv) with
         | [] -> ["."]
         | dirs -> dirs);
    Printf.printf "%s contains %d bytes\n"
      (String.concat " " (List.tl (Array.to_list Sys.argv))) !sum
  
  (*-----------------------------*)
  
  (* Find the largest file in a directory tree. *)
  #load "unix.cma";;
  let saved_size = ref 0
  let saved_name = ref ""
  let () =
    List.iter
      (find_files
         (fun fn ->
            (match Unix.stat fn with
               | {Unix.st_size=size} ->
                   if size > !saved_size
                   then (saved_size := size; saved_name := fn));
            true)
         (fun _ _ -> ()))
      (match List.tl (Array.to_list Sys.argv) with
         | [] -> ["."]
         | dirs -> dirs);
    Printf.printf "Biggest file %s in %s is %d bytes long.\n"
      !saved_name
      (String.concat " " (List.tl (Array.to_list Sys.argv)))
      !saved_size
  
  (*-----------------------------*)
  
  (* Find the youngest file or directory. *)
  #load "unix.cma";;
  let saved_age = ref 0.
  let saved_name = ref ""
  let () =
    List.iter
      (find_files
         (fun fn ->
            (match Unix.stat fn with
               | {Unix.st_mtime=age} ->
                   if age > !saved_age
                   then (saved_age := age; saved_name := fn));
            true)
         (fun _ _ -> ()))
      (match List.tl (Array.to_list Sys.argv) with
         | [] -> ["."]
         | dirs -> dirs);
    match Unix.localtime !saved_age with
      | {Unix.tm_year=year; tm_mon=month; tm_mday=day} ->
          Printf.printf "%04d-%02d-%02d %s\n"
            (year + 1900) (month + 1) day
            !saved_name
  
  (*-----------------------------*)
  
  (* fdirs - find all directories *)
  let () =
    List.iter
      (find_files
         (fun fn ->
            if Sys.is_directory fn then print_endline fn;
            true)
         (fun _ _ -> ()))
      (match List.tl (Array.to_list Sys.argv) with
         | [] -> ["."]
         | dirs -> dirs)
  

