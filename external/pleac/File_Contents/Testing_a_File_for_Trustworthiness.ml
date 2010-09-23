(* ********************************************************************** *)
(* Testing a File for Trustworthiness *)
(* ********************************************************************** *)
let pleac_Testing_a_File_for_Trustworthiness () = 
  #load "unix.cma";;
  
  let () =
    try
      let {Unix.st_dev = dev;
           st_ino = ino;
           st_kind = kind;
           st_perm = perm;
           st_nlink = nlink;
           st_uid = uid;
           st_gid = gid;
           st_rdev = rdev;
           st_size = size;
           st_atime = atime;
           st_mtime = mtime;
           st_ctime = ctime} = Unix.stat filename in
      (* ... *)
      ()
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "no %s: %s\n" filename (Unix.error_message e);
      exit 0
  
  (*-----------------------------*)
  
  let () =
    let info =
      try Unix.stat filename
      with Unix.Unix_error (e, _, _) ->
        Printf.eprintf "no %s: %s\n" filename (Unix.error_message e);
        exit 0 in
    if info.Unix.st_uid = 0
    then Printf.printf "Superuser owns %s\n" filename;
    if info.Unix.st_atime > info.Unix.st_mtime
    then Printf.printf "%s has been read since it was written.\n" filename
  
  (*-----------------------------*)
  
  let is_safe path =
    let info = Unix.stat path in
    (* owner neither superuser nor me *)
    (* the real uid can be retrieved with Unix.getuid () *)
    if (info.Unix.st_uid <> 0) && (info.Unix.st_uid <> Unix.getuid ())
    then false
    else
      (* check whether the group or other can write file. *)
      (* use 0o066 to detect either reading or writing *)
      if info.Unix.st_perm land 0o022 = 0
      then true  (* no one else can write this *)
      else if info.Unix.st_kind <> Unix.S_DIR
      then false (* non-directories aren't safe *)
      else if info.Unix.st_perm land 0o1000 <> 0
      then true  (* but directories with the sticky bit (0o1000) are *)
      else false
  
  (*-----------------------------*)
  
  let is_verysafe path =
    let rec loop path parent =
      if not (is_safe path)
      then false
      else if path <> parent
      then loop parent (Filename.dirname parent)
      else true in
    loop path (Filename.dirname path)
  

