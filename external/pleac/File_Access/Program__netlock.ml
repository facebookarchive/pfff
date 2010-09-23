(* ********************************************************************** *)
(* Program: netlock *)
(* ********************************************************************** *)
let pleac_Program__netlock () = 
  drivelock.ml:
  
  #!/usr/bin/ocaml
  (* drivelock - demo LockDir module *)
  #use "netlock.ml";;
  let die msg = prerr_endline msg; exit 1
  let () =
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle (fun _ -> die "outta here"));
    LockDir.debug := true;
    let path =
      try Sys.argv.(1)
      with Invalid_argument _ ->
        die ("usage: " ^ Sys.argv.(0) ^ " <path>") in
    (try LockDir.nflock ~naptime:2 path
     with LockDir.Error _ ->
       die ("couldn't lock " ^ path ^ " in 2 seconds"));
    Unix.sleep 100;
    LockDir.nunflock path
  
  (*-----------------------------*)
  
  netlock.ml:
  
  #load "unix.cma";;
  
  (* module to provide very basic filename-level *)
  (* locks.  No fancy systems calls.  In theory, *)
  (* directory info is sync'd over NFS.  Not *)
  (* stress tested. *)
  
  module LockDir :
  sig
  
    exception Error of string
  
    val debug : bool ref
    val check : int ref
    val nflock : ?naptime:int -> string -> unit
    val nunflock : string -> unit
  
  end = struct
  
    exception Error of string
  
    let debug = ref false
    let check = ref 1
  
    module StringSet = Set.Make(String)
    let locked_files = ref StringSet.empty
  
    (* helper function *)
    let name2lock pathname =
      let dir = Filename.dirname pathname in
      let file = Filename.basename pathname in
      let dir = if dir = "." then Sys.getcwd () else dir in
      let lockname = Filename.concat dir (file ^ ".LOCKDIR") in
      lockname
  
    let nflock ?(naptime=0) pathname =
      let lockname = name2lock pathname in
      let whosegot = Filename.concat lockname "owner" in
      let start = Unix.time () in
      let missed = ref 0 in
  
      (* if locking what I've already locked, raise exception *)
      if StringSet.mem pathname !locked_files
      then raise (Error (pathname ^ " already locked"));
  
      Unix.access (Filename.dirname pathname) [Unix.W_OK];
  
      begin
        try
          while true do
            try
              Unix.mkdir lockname 0o777;
              raise Exit
            with Unix.Unix_error (e, _, _) ->
              incr missed;
              if !missed > 10
              then raise (Error
                            (Printf.sprintf "can't get %s: %s"
                               lockname (Unix.error_message e)));
              if !debug
              then
                begin
                  let owner = open_in whosegot in
                  let lockee = input_line owner in
                  close_in owner;
                  Printf.eprintf "%s[%d]: lock on %s held by %s\n%!"
                    Sys.argv.(0) (Unix.getpid ()) pathname lockee
                end;
              Unix.sleep !check;
              if naptime > 0 && Unix.time () > start +. float naptime
              then raise Exit
          done
        with Exit -> ()
      end;
  
      let owner =
        try
          open_out_gen [Open_wronly; Open_creat; Open_excl] 0o666 whosegot
        with Sys_error e ->
          raise (Error ("can't create " ^ e)) in
      Printf.fprintf owner "%s[%d] on %s\n"
        Sys.argv.(0) (Unix.getpid ()) (Unix.gethostname ());
      close_out owner;
      locked_files := StringSet.add pathname !locked_files
  
    (* free the locked file *)
    let nunflock pathname =
      let lockname = name2lock pathname in
      let whosegot = Filename.concat lockname "owner" in
      Unix.unlink whosegot;
      if !debug then Printf.eprintf "releasing lock on %s\n%!" lockname;
      locked_files := StringSet.remove pathname !locked_files;
      Unix.rmdir lockname
  
    (* anything forgotten? *)
    let () =
      at_exit
        (fun () ->
           StringSet.iter
             (fun pathname ->
                let lockname = name2lock pathname in
                let whosegot = Filename.concat lockname "owner" in
                Printf.eprintf "releasing forgotten %s\n%!" lockname;
                Unix.unlink whosegot;
                Unix.rmdir lockname)
             !locked_files)
  end
  

