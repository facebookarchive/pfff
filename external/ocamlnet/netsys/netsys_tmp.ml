(* $Id: netsys_tmp.ml 1518 2010-12-19 19:33:45Z gerd $ *)

open Printf

let tmp_dir = ref None

let tmp_dir_mutex = !Netsys_oothr.provider # create_mutex()

let tmp_directory() =
  match !tmp_dir with
    | None ->
	Netsys_oothr.serialize
	  tmp_dir_mutex
	  (fun () ->
	     try
	       let envvar =
		 match Sys.os_type with
		   | "Unix" | "Cygwin" -> "TMPDIR"
		   | "Win32" -> "TEMP"
		   | _ -> raise Not_found in
	       let d = Sys.getenv envvar in
	       if not(Sys.file_exists d) then raise Not_found;
	       tmp_dir := Some d;
	       d
	     with
	       | Not_found ->
		   let candidates =
		     match Sys.os_type with
		       | "Unix" | "Cygwin" -> [ "/tmp"; "/var/tmp"; "." ]
		       | "Win32" -> [ "C:\\TEMP"; "." ]
		       | _ -> assert false in
		   let d = 
		     try List.find Sys.file_exists candidates
		     with Not_found -> assert false in
		   tmp_dir := Some d;
		   d
	  )
	  ()
    | Some d -> d


let set_tmp_directory d =
  tmp_dir := Some d


let counter = ref 0

let tmp_prefix p =
  let c = !counter in
  incr counter;
  let s =
    Digest.string(sprintf "%d/%f/%d"
		    (Unix.getpid())
		    (Gc.quick_stat()).Gc.minor_words
		    c) in
  let hex = Digest.to_hex s in
  p ^ String.sub hex 0 8
