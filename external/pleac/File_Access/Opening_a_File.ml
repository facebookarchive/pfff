(* ********************************************************************** *)
(* Opening a File *)
(* ********************************************************************** *)
let pleac_Opening_a_File () = 
  (* open file "path" for reading only *)
  let source =
    try open_in path
    with Sys_error msg -> failwith ("Couldn't read from " ^ msg)
  
  (* open file "path" for writing only *)
  let sink =
    try open_out path
    with Sys_error msg -> failwith ("Couldn't write to " ^ msg)
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  
  (* open file "path" for reading only *)
  let source =
    try Unix.openfile path [Unix.O_RDONLY] 0o644
    with Unix.Unix_error (code, func, param) ->
      failwith (Printf.sprintf "Couldn't open %s for reading: %s"
                  path (Unix.error_message code))
  
  (* open file "path" for writing only *)
  let sink =
    try Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT] 0o644
    with Unix.Unix_error (code, func, param) ->
      failwith (Printf.sprintf "Couldn't open %s for writing: %s"
                  path (Unix.error_message code))
  
  (*-----------------------------*)
  
  (* open file "path" for reading and writing *)
  let fh =
    try Unix.openfile filename [Unix.O_RDWR] 0o644
    with Unix.Unix_error (code, func, param) ->
      failwith (Printf.sprintf "Couldn't open %s for read and write: %s"
                  filename (Unix.error_message code))
  
  (*-----------------------------*)
  
  (* open file "path" read only *)
  let fh = open_in path
  let fh = Unix.openfile path [Unix.O_RDONLY] 0o644
  
  (*-----------------------------*)
  
  (* open file "path" write only, create it if it does not exist *)
  let fh = open_out path
  let fh = Unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT] 0o600
  
  (*-----------------------------*)
  
  (* open file "path" write only, fails if file exists *)
  let fh = Unix.openfile path [Unix.O_WRONLY; Unix.O_EXCL; Unix.O_CREAT] 0o600
  
  (*-----------------------------*)
  
  (* open file "path" for appending *)
  let fh =
    open_out_gen [Open_wronly; Open_append; Open_creat] 0o600 path
  let fh =
    Unix.openfile path [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 0o600
  
  (*-----------------------------*)
  
  (* open file "path" for appending only when file exists *)
  let fh = Unix.openfile path [Unix.O_WRONLY; Unix.O_APPEND] 0o600
  
  (*-----------------------------*)
  
  (* open file "path" for reading and writing *)
  let fh = Unix.openfile path [Unix.O_RDWR] 0o600
  
  (*-----------------------------*)
  
  (* open file "path" for reading and writing,
     create a new file if it does not exist *)
  let fh = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o600
  
  (*-----------------------------*)
  
  (* open file "path" for reading and writing, fails if file exists *)
  let fh = Unix.openfile path [Unix.O_RDWR; Unix.O_EXCL; Unix.O_CREAT] 0o600
  

