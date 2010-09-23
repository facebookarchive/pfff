(* ********************************************************************** *)
(* Program: lst *)
(* ********************************************************************** *)
let pleac_Program__lst () = 
  #!/usr/bin/ocaml
  (* lst - list sorted directory contents (depth first) *)
  #load "unix.cma";;
  
  open Unix
  open Printf
  
  let opt_m = ref false
  let opt_u = ref false
  let opt_c = ref false
  let opt_s = ref false
  let opt_r = ref false
  let opt_i = ref false
  let opt_l = ref false
  let names = ref []
  
  let () =
    Arg.parse
      [
        "-m", Arg.Set opt_m, "Use mtime (modify time) [DEFAULT]";
        "-u", Arg.Set opt_u, "Use atime (access time)";
        "-c", Arg.Set opt_c, "Use ctime (inode change time)";
        "-s", Arg.Set opt_s, "Use size for sorting";
        "-r", Arg.Set opt_r, "Reverse sort";
        "-i", Arg.Set opt_i, "Read pathnames from stdin";
        "-l", Arg.Set opt_l, "Long listing";
      ]
      (fun name -> names := name :: !names)
      (sprintf
         "Usage: %s [-m] [-u] [-c] [-s] [-r] [-i] [-l] [dirs ...]
   or    %s -i [-m] [-u] [-c] [-s] [-r] [-l] < filelist"
         Sys.argv.(0) Sys.argv.(0));
    names :=
      match !names with
        | [] when not !opt_i -> ["."]
        | names -> names
  
  let die msg = prerr_endline msg; exit 1
  
  let () =
    let int_of_bool = function true -> 1 | false -> 0 in
    if (int_of_bool !opt_c
        + int_of_bool !opt_u
        + int_of_bool !opt_s
        + int_of_bool !opt_m) > 1
    then die "can only sort on one time or size"
  
  let idx = fun {st_mtime=t} -> t
  let idx = if !opt_u then fun {st_atime=t} -> t else idx
  let idx = if !opt_c then fun {st_ctime=t} -> t else idx
  let idx = if !opt_s then fun {st_size=s} -> float s else idx
  let time_idx = if !opt_s then fun {st_mtime=t} -> t else idx
  
  let rec find f roots =
    Array.iter
      (fun root ->
         f root;
         match lstat root with
           | {st_kind=S_DIR} ->
               find f (Array.map
                         (Filename.concat root)
                         (Sys.readdir root))
           | _ -> ())
      roots
  
  let time = Hashtbl.create 0
  let stat = Hashtbl.create 0
  
  (* get stat info on the file, saving the desired *)
  (* sort criterion (mtime, atime, ctime, or size) *)
  (* in the time hash indexed by filename.         *)
  (* if they want a long list, we have to save the *)
  (* entire stat structure in stat.                *)
  let wanted name =
    try
      let sb = Unix.stat name in
      Hashtbl.replace time name (idx sb);
      if !opt_l then Hashtbl.replace stat name sb
    with Unix_error _ -> ()
  
  (* cache user number to name conversions *)
  let user =
    let user = Hashtbl.create 0 in
    fun uid ->
      Hashtbl.replace user uid
        (try (getpwuid uid).pw_name
         with Not_found -> ("#" ^ string_of_int uid));
      Hashtbl.find user uid
  
  (* cache group number to name conversions *)
  let group =
    let group = Hashtbl.create 0 in
    fun gid ->
      Hashtbl.replace group gid
        (try (getgrgid gid).gr_name
         with Not_found -> ("#" ^ string_of_int gid));
      Hashtbl.find group gid
  
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                  "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  
  let format_time time =
    let tm = localtime time in
    sprintf "%s %s %2d %02d:%02d:%02d %04d"
      days.(tm.tm_wday)
      months.(tm.tm_mon)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
      (tm.tm_year + 1900)
  
  let () =
    if !opt_i
    then
      begin
        begin
          try
            while true do
              names := (input_line Pervasives.stdin) :: !names
            done
          with End_of_file -> ()
        end;
        List.iter wanted (List.rev !names)
      end
    else find wanted (Array.of_list (List.rev !names))
  
  (* sort the files by their cached times, youngest first *)
  let skeys =
    List.sort
      (fun a b -> compare (Hashtbl.find time b) (Hashtbl.find time a))
      (Hashtbl.fold (fun k v a -> k :: a) time [])
  
  (* but flip the order if -r was supplied on command line *)
  let skeys = if !opt_r then List.rev skeys else skeys
  
  let () =
    List.iter
      (fun skey ->
         if !opt_l
         then
           let sb = Hashtbl.find stat skey in
           printf "%6d %04o %6d %8s %8s %8d %s %s\n"
             sb.st_ino
             (sb.st_perm land 0o7777)
             sb.st_nlink
             (user sb.st_uid)
             (group sb.st_gid)
             sb.st_size
             (format_time (time_idx sb))
             skey
         else
           print_endline skey)
      skeys
  
  

