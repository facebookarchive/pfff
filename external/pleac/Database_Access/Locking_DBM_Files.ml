(* ********************************************************************** *)
(* Locking DBM Files *)
(* ********************************************************************** *)
let pleac_Locking_DBM_Files () = 
  (* dblockdemo - demo locking dbm databases *)
  (* Thanks to Janne Hellsten for posting sample code on caml-list! *)
  
  #load "dbm.cma";;
  #load "unix.cma";;
  
  let db_file = "/tmp/foo.db"
  let lock_file = "/tmp/foo.lock"
  
  let key = try Sys.argv.(1) with Invalid_argument _ -> "default"
  let value = try Sys.argv.(2) with Invalid_argument _ -> "magic"
  let value = value ^ " " ^ (string_of_int (Unix.getpid ()))
  
  let finally handler f x =
    let result = try f x with e -> handler (); raise e in handler (); result
  
  let create_lock name =
    if not (Sys.file_exists name) then
      let out_channel = open_out name in close_out out_channel
  
  let with_lock name command f =
    create_lock name;
    let fd = Unix.openfile name [Unix.O_RDWR] 0o660 in
    finally
      (fun () -> Unix.close fd)
      (fun () -> Unix.lockf fd command 0; f ()) ()
  
  let create_db name =
    if not (Sys.file_exists (name ^ ".dir")) then
      let db = Dbm.opendbm name [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o660 in
      Dbm.close db
  
  let () =
    create_db db_file;
  
    let do_read () =
      let db = Dbm.opendbm db_file [Dbm.Dbm_rdonly] 0o660 in
      Printf.printf "%d: Read lock granted\n" (Unix.getpid ());
      flush stdout;
      let oldval = try Dbm.find db key with Not_found -> "" in
      Printf.printf "%d: Old value was %s\n" (Unix.getpid ()) oldval;
      flush stdout;
      Dbm.close db in
  
    let do_write () =
      let db = Dbm.opendbm db_file [Dbm.Dbm_rdwr] 0o660 in
      Printf.printf "%d: Write lock granted\n" (Unix.getpid ());
      flush stdout;
      Dbm.replace db key value;
      Unix.sleep 10;
      Dbm.close db in
  
    begin
      try
        with_lock lock_file Unix.F_TRLOCK do_read;
      with Unix.Unix_error (error, "lockf", _) ->
        Printf.printf "%d: CONTENTION; can't read during write update! \
                           Waiting for read lock (%s) ...\n"
          (Unix.getpid ()) (Unix.error_message error);
        flush stdout;
        with_lock lock_file Unix.F_RLOCK do_read
    end;
  
    begin
      try 
        with_lock lock_file Unix.F_TLOCK do_write;
      with Unix.Unix_error (error, "lockf", _) ->
        Printf.printf "%d: CONTENTION; must have exclusive lock! \
                           Waiting for write lock (%s) ...\n"
          (Unix.getpid ()) (Unix.error_message error);
        flush stdout;
        with_lock lock_file Unix.F_LOCK do_write
    end;
  
    Printf.printf "%d: Updated db to %s=%s\n" (Unix.getpid ()) key value
  

