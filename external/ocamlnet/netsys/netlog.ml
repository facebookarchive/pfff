(* $Id: netlog.ml 1277 2009-10-13 00:03:45Z gerd $ *)

open Printf

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

type logger =
    level -> string -> unit

let level_weight =
  function
    | `Emerg   -> 0
    | `Alert   -> 1
    | `Crit    -> 2
    | `Err     -> 3
    | `Warning -> 4
    | `Notice  -> 5
    | `Info    -> 6
    | `Debug   -> 7

let level_names =
  [| "emerg"; "alert"; "crit"; "err"; "warning"; "notice"; "info"; "debug" |]

let string_of_level lev =
  level_names.( level_weight lev )


let level_of_string s =
  let s = String.lowercase s in
  match s with
    | "emerg"   -> `Emerg
    | "alert"   -> `Alert
    | "crit"    -> `Crit
    | "err"     -> `Err
    | "warning" -> `Warning
    | "notice"  -> `Notice
    | "info"    -> `Info
    | "debug"   -> `Debug
    | _         -> failwith ("Unknown level: " ^ s)

let weekday =
  [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let month =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
     "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let channel_logger ch max_lev lev msg = 
  if level_weight lev <= level_weight max_lev then (
    let t = Unix.localtime(Unix.gettimeofday()) in
    let s =   (* Netdate is unavailable here *)
      sprintf
	"[%s %s %2d %02d:%02d:%02d %4d] [%s] %s%s\n"
	weekday.(t.Unix.tm_wday)
	month.(t.Unix.tm_mon)
	t.Unix.tm_mday
	t.Unix.tm_hour
	t.Unix.tm_min
	t.Unix.tm_sec
	(1900 + t.Unix.tm_year)
	(string_of_level lev)
	( match lev with
	    | `Debug ->
		sprintf "[%d:%d] " 
		  (Unix.getpid())
		  (!Netsys_oothr.provider # self # id)
	    | _ -> ""
	)
	msg in
    output_string ch s;
    flush ch
  )
    

let current_logger =
  ref(channel_logger Pervasives.stderr `Debug)


let log lev msg =
  !current_logger lev msg

let logf level fmt =
  Printf.ksprintf (log level) fmt

module Debug = struct
  type dlogger =
      string -> string -> unit

  let fwd_dlogger mname msg =
    log `Debug (mname ^ ": " ^ msg)

  let null_dlogger _ _ = ()

  let current_dlogger =
    ref fwd_dlogger

  let log mname msg =
    !current_dlogger mname msg

  let logf mname fmt =
    Printf.ksprintf (log mname) fmt

  let registry = Hashtbl.create 11

  let register_module mname evar =
    Hashtbl.replace registry mname evar

  let set_module mname b =
    try
      let evar = Hashtbl.find registry mname in
      evar := b
    with Not_found -> ()

  let set_all_modules b =
    Hashtbl.iter
      (fun _ evar -> evar := b)
      registry

  let enable_module mname =
    set_module mname true

  let disable_module mname =
    set_module mname false

  let enable_all () =
    set_all_modules true

  let disable_all () =
    set_all_modules false

  let names() =
    List.sort
      compare
      (Hashtbl.fold (fun name _ acc -> name::acc) registry [])

  let mk_dlog mname enable msg =
    if !enable then
      log mname msg

  let mk_dlogr mname enable f =
    if !enable then
      log mname (f())

  external int64_of_file_descr : Unix.file_descr -> int64
    = "netsys_int64_of_file_descr"
    (* Also occurs in netsys_win32.ml! *)

  type serial = < > ;;

  let new_serial() = (object end)

  let enable_fd_tracking = ref false

  let fd_tab = Hashtbl.create 50
  let fd_tab_mutex = !Netsys_oothr.provider # create_mutex()

  let fd_string_1 ?(owner=false) ?(descr=false) fd =
    try
      let (owner_s, descr_s, sn_opt, anchor_entry) =
	Hashtbl.find fd_tab fd in
      sprintf "%Ld(%s%s%s)"
	(int64_of_file_descr fd)
	(if owner then owner_s else "")
	(if owner && descr then " - " else "")
	(if descr then descr_s else "")
    with
      | Not_found ->
	  sprintf "%Ld(?)" (int64_of_file_descr fd)


  let finalise_anchor r _ =
    r := true

  let tracker =
    "Netlog"

  let track_fd ?(update=false) ?anchor ?sn ~owner ~descr fd =
    let anchor_entry =
      match anchor with
	| None -> None
	| Some x ->
	    let r = ref false in
	    Gc.finalise (finalise_anchor r) x;
	    Some r
    in
    Netsys_oothr.serialize
      fd_tab_mutex
      (fun () ->
	 if update then (
	   let verbose =
	     if Hashtbl.mem fd_tab fd then (
	       let (_, _, old_sn_opt, _) = Hashtbl.find fd_tab fd in
	       if old_sn_opt <> None && old_sn_opt <> sn then (
		 logf tracker "WARNING track_fd: escriptor already tracked \
                               with different sn as %s"
		   (fd_string_1 ~owner:true ~descr:true fd);
		 true
	       )
	       else !enable_fd_tracking
	     )
	     else !enable_fd_tracking in
	   Hashtbl.replace fd_tab fd (owner, descr, sn, anchor_entry);
	   if verbose then
	     logf tracker "track_fd: updating tracked descriptor %s"
	       (fd_string_1 ~owner:true ~descr:true fd)
	 )
	 else (
	   let verbose =
	     if Hashtbl.mem fd_tab fd then (
	       logf tracker "WARNING track_fd: descriptor already tracked as %s"
		 (fd_string_1 ~owner:true ~descr:true fd);
	       true
	     ) 
	     else !enable_fd_tracking in
	   Hashtbl.replace fd_tab fd (owner, descr, sn, anchor_entry);
	   if verbose then
	     logf tracker "track_fd: tracking descriptor %s"
	       (fd_string_1 ~owner:true ~descr:true fd)
	 )
      )
      ()

  let release_fd ?sn ?(force=false) fd =
    Netsys_oothr.serialize
      fd_tab_mutex
      (fun () ->
	 try
	   let (_, _, old_sn_opt, _) = Hashtbl.find fd_tab fd in
	   let verbose =
	     if old_sn_opt <> None && old_sn_opt <> sn && not force then (
	       logf tracker "WARNING release_fd: Descriptor is tracked \
                             with unexpected sn as %s"
		 (fd_string_1 ~owner:true ~descr:true fd);
	       true
	     )
	     else !enable_fd_tracking in
	   if verbose then
	     logf tracker "release_fd: releasing descriptor %s"
	       (fd_string_1 ~owner:true ~descr:true fd);
	   Hashtbl.remove fd_tab fd;
	 with
	   | Not_found ->
	       if not force then
		 logf tracker "WARNING release_fd: no such descriptor %s"
		   (fd_string_1 fd)
      )
      ()

  let fd_string ?owner ?descr fd =
    Netsys_oothr.serialize
      fd_tab_mutex
      (fun () -> fd_string_1 ?owner ?descr fd)
      ()

  let fd_table () =
    Netsys_oothr.serialize
      fd_tab_mutex
      (fun () ->
	 let tab =
	   Hashtbl.fold
	     (fun fd (owner,descr,_,anchor_flag) acc ->
		let n = int64_of_file_descr fd in
		let line =
		  sprintf "%4Ld  %-15s %-15s %s"
		    n
		    owner
		    descr
		    (match anchor_flag with
		       | Some flag -> if !flag then "DEAD" else ""
		       | _ -> ""
		    ) in
		(n,line) :: acc
	     )
	     fd_tab
	     [] in
	 let tab' =
	   List.sort (fun (n1,_) (n2,_) -> Int64.compare n1 n2) tab in
	 List.map snd tab'
      )
      ()
    
end
