(* ********************************************************************** *)
(* Doing Non-Blocking I/O *)
(* ********************************************************************** *)
let pleac_Doing_Non_Blocking_I_O () = 
  #load "unix.cma";;
  
  (* Pass the O_NONBLOCK flag when calling Unix.openfile. *)
  let file_descr =
    try Unix.openfile "/dev/cua0" [Unix.O_RDWR; Unix.O_NONBLOCK] 0o666
    with Unix.Unix_error (code, func, param) ->
      Printf.eprintf "Can't open modem: %s\n" (Unix.error_message code);
      exit 2
  
  (*-----------------------------*)
  
  (* If the file descriptor already exists, use Unix.set_nonblock. *)
  let () = Unix.set_nonblock file_descr
  
  (*-----------------------------*)
  
  (* In non-blocking mode, calls that would block throw exceptions. *)
  let () =
    let chars_written =
      try
        Some (Unix.single_write file_descr buffer 0 (String.length buffer))
      with
        | Unix.Unix_error (Unix.EAGAIN, _, _)
        | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> None in
    match chars_written with
      | Some n when n = String.length buffer ->
          (* successfully wrote *)
          ()
      | Some n ->
          (* incomplete write *)
          ()
      | None ->
          (* would block *)
          ()
  
  let () =
    let chars_read =
      try
        Some (Unix.read file_descr buffer 0 buffer_size)
      with
        | Unix.Unix_error (Unix.EAGAIN, _, _)
        | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> None in
    match chars_read with
      | Some n ->
          (* successfully read n bytes from file_descr *)
          ()
      | None ->
          (* would block *)
          ()
  

