(* $Id: netfs.ml 1520 2010-12-22 23:52:22Z gerd $ *)

type read_flag =
    [ `Skip of int64 | `Binary | `Streaming | `Dummy ]

type write_flag =
    [ `Create | `Exclusive | `Truncate | `Binary | `Streaming | `Dummy ]

type size_flag =
    [ `Dummy ]

type test_flag =
    [ `Link | `Dummy ]

type remove_flag =
    [ `Recursive | `Dummy ]

type rename_flag =
    [ `Dummy ]

type symlink_flag =
    [ `Dummy ]

type readdir_flag =
    [ `Dummy ]

type readlink_flag =
    [ `Dummy ]

type mkdir_flag =
    [ `Path | `Nonexcl | `Dummy ]

type rmdir_flag =
    [ `Dummy ]

type copy_flag =
    [ `Dummy ]

type test_type =
    [ `N | `E | `D | `F | `H | `R | `W | `X | `S ]

class type stream_fs =
object
  method path_encoding : Netconversion.encoding option
  method path_exclusions : (int * int) list
  method nominal_dot_dot : bool
  method read : read_flag list -> string -> Netchannels.in_obj_channel
  method write : write_flag list -> string -> Netchannels.out_obj_channel
  method size : size_flag list -> string -> int64
  method test : test_flag list -> string -> test_type -> bool
  method test_list : test_flag list -> string -> test_type list -> bool list
  method remove : remove_flag list -> string -> unit
  method rename : rename_flag list -> string -> string -> unit
  method symlink : symlink_flag list -> string -> string -> unit
  method readdir : readdir_flag list -> string -> string list
  method readlink : readlink_flag list -> string -> string
  method mkdir : mkdir_flag list -> string -> unit
  method rmdir : rmdir_flag list -> string -> unit
  method copy : copy_flag list -> string -> string -> unit
end


let slash_re = Netstring_pcre.regexp "/+"

let drive_re = Netstring_pcre.regexp "^[a-zA-Z]:$"

exception Not_absolute
exception Unavailable

let list_isect_empty l1 l2 = (* whether intersection is empty *)
  List.for_all
    (fun x1 -> not (List.mem x1 l2))
    l1

let readdir d =
  try
    let l = ref [] in
    ( try
	while true do
	  l := (Unix.readdir d) :: !l
	done;
	assert false
      with End_of_file -> ()
    );
    Unix.closedir d;
    List.rev !l
  with
    | error -> Unix.closedir d; raise error

let copy_prim ~streaming orig_fs orig_name dest_fs dest_name =
  let sflags = 
    if streaming then [`Streaming] else []  in
  Netchannels.with_in_obj_channel
    (orig_fs#read (sflags @ [`Binary]) orig_name)
    (fun r_ch ->
       Netchannels.with_out_obj_channel
	 (dest_fs#write (sflags @ [`Binary; `Truncate; `Create]) dest_name)
	 (fun w_ch ->
	    w_ch # output_channel r_ch
	 )
    )
    
      
let local_fs ?encoding ?root () : stream_fs =
  let enc =
    match encoding with
      | None ->
	  ( match Sys.os_type with
	      | "Win32" -> Netconversion.user_encoding()
	      | _ -> None
	  )
      | Some e -> Some e in
  ( match enc with
      | None -> ()
      | Some e ->
	  if not (Netconversion.is_ascii_compatible e) then
	    failwith
	      "Netfs.local_fs: the encoding is not ASCII-compatible";
  );
  let excl =
    match Sys.os_type with
      | "Win32" | "Cygwin" ->
	  (* http://msdn.microsoft.com/en-us/library/aa365247%28v=VS.85%29.aspx *)
	  [ 0, 31;  (* control chars *)
	    42, 42; (* <, >, :, quotation mark, /, backslash, |, ?, * *)
	    47, 47;
	    58, 58;
	    60, 60;
	    62, 63;
	    92, 92;
	    124, 124
	  ]
      | _ ->
	  [ 0, 0; 47, 47 ] in
  let excl_array_size =
    List.fold_left (fun mx (from,upto) -> max mx upto) 0 excl + 1 in
  let excl_array = (
    let a = Array.make excl_array_size false in
    List.iter
      (fun (from,upto) ->
	 for k = from to upto do a.(k) <- true done
      )
      excl;
    a) in

  let check_component path c =
    let iter f s =
      match enc with
	| None -> 
	    String.iter (fun c -> f (Char.code c)) s
	| Some e -> 
	    Netconversion.ustring_iter e f s in
    try
      iter
	(fun code ->
	   if code < excl_array_size && excl_array.(code) then
	     raise (Unix.Unix_error(Unix.EINVAL,
				    "Netfs: invalid char in path",
				    path))
	)
	c
    with Netconversion.Malformed_code ->
      raise (Unix.Unix_error(Unix.EINVAL,
			     "Netfs: path does not comply to charset encoding",
			     path)) in
	  
  let win32_root =
    root = None && Sys.os_type = "Win32" in

  let is_drive_letter s =
    Netstring_pcre.string_match drive_re s 0 <> None in

  let is_unc s =
    String.length s >= 3 && s.[0] = '/' && s.[1] = '/' && s.[2] <> '/' in

  let check_and_norm_path p =
    try
      let l = Netstring_pcre.split_delim slash_re p in
      ( match l with
	  | [] ->  raise (Unix.Unix_error(Unix.EINVAL,
					  "Netfs: empty path",
					  p))
	  | "" :: first :: rest ->
	      if win32_root then (
		if ((not (is_drive_letter first) || rest=[]) && 
		     not (is_unc p))
		then
		  raise Not_absolute
	      )
	  | first :: rest ->
	      if win32_root then (
		if not(is_drive_letter first) || rest=[] then
		  raise Not_absolute
	      )
	      else raise Not_absolute
      );
      List.iter (check_component p) l;
      let np = String.concat "/" l in
      if win32_root then (
	if is_unc p then
	  "/" ^ np
	else
	  if np.[0] = '/' then
	    String.sub np 1 (String.length np - 1)  (* remove leading / *)
	  else np
      )
      else np
    with 
      | Not_absolute ->
	  raise (Unix.Unix_error(Unix.EINVAL,
				 "Netfs: path not absolute",
				 p))
  in

  let real_root =
    match root with
      | None ->
	  ""
      | Some r ->
	  if (Unix.stat r).Unix.st_kind <> Unix.S_DIR then
	    failwith "Netfs.local_fs: root is not a directory";
	  r in

  ( object(self)
      method path_encoding = enc
      method path_exclusions = excl
      method nominal_dot_dot = false

      method read flags filename =
	let fn = real_root ^ check_and_norm_path filename in
	let binary = List.mem `Binary flags in
	let skip_d = 
	  try
	    List.find
	      (fun flag -> 
		 match flag with
		   | `Skip _ -> true
		   | _ -> false
	      ) 
	      flags 
	  with Not_found -> `Skip 0L in
	let skip =
	  match skip_d with
	    | `Skip n -> n
	    | _ -> assert false in
	(* Use Unix.openfile to open so we get Unix_errors on error *)
	let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in
	let st = Unix.fstat fd in
	if st.Unix.st_kind = Unix.S_DIR then
	  raise(Unix.Unix_error(Unix.EISDIR,"Netfs.read",""));
	if skip > 0L then
	  ignore(Unix.LargeFile.lseek fd skip Unix.SEEK_SET);
	let ch = Unix.in_channel_of_descr fd in
	set_binary_mode_in ch binary;
	new Netchannels.input_channel ch

      method write flags filename =
	let fn = real_root ^ check_and_norm_path filename in
	let binary = List.mem `Binary flags in
	let create = List.mem `Create flags in
	let truncate = List.mem `Truncate flags in
	let exclusive = List.mem `Exclusive flags in
	let mode =
	  List.flatten
	    [ [Unix.O_WRONLY];
	      if create then [ Unix.O_CREAT ] else [];
	      if truncate then [ Unix.O_TRUNC ] else [];
	      if exclusive then [ Unix.O_EXCL ] else [];
	    ] in
	(* Use Unix.openfile to open so we get Unix_errors on error *)
	let fd = Unix.openfile fn mode 0o666 in
	let ch = Unix.out_channel_of_descr fd in
	set_binary_mode_out ch binary;
	new Netchannels.output_channel ch

      method size flags filename =
	let fn = real_root ^ check_and_norm_path filename in
	let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in
	try
	  let n = Unix.LargeFile.lseek fd 0L Unix.SEEK_END in
	  Unix.close fd;
	  n
	with
	  | error -> Unix.close fd; raise error  (* esp. non-seekable *)

      method private test_list_NH flags fn =
	try
	  let st = Unix.LargeFile.lstat fn in
	  if st.Unix.LargeFile.st_kind = Unix.S_LNK then
	    [ `N; `H ]
	  else
	    [ `N ]
	with
	  | Unix.Unix_error(Unix.ENOENT,_,_) -> []

      method private test_list_EDFS flags fn =
	try
	  let st = Unix.LargeFile.stat fn in
	  let non_empty = st.Unix.LargeFile.st_size <> 0L in
	  let kind_l =
	    match st.Unix.LargeFile.st_kind with
	      | Unix.S_REG -> [ `F ]
	      | Unix.S_DIR -> [ `D ]
	      | _ -> [] in
	  [ `E ] @ kind_l @ (if non_empty then [`S] else [])
	with
	  | Unix.Unix_error(Unix.ENOENT,_,_) -> []

      method private test_list_RWX flags fn =
	let r_ok =
	  try Unix.access fn [Unix.R_OK]; true with _ -> false in
	let w_ok =
	  try Unix.access fn [Unix.W_OK]; true with _ -> false in
	let x_ok =
	  try Unix.access fn [Unix.X_OK]; true with _ -> false in
	List.flatten
	  [ if r_ok then [`R] else [];
	    if w_ok then [`W] else [];
	    if x_ok then [`X] else []
	  ]

      method test flags filename ttype =
	let fn = real_root ^ check_and_norm_path filename in
	let l =
	  match ttype with
	    | `N | `H -> self#test_list_NH flags fn
	    | `E | `D | `F | `S -> self#test_list_EDFS flags fn
	    | `R | `W | `X -> self#test_list_RWX flags fn in
	List.mem ttype l

      method test_list flags filename tests =
	let fn = real_root ^ check_and_norm_path filename in
	let nh =
	  if not(list_isect_empty tests [`N;`H]) then
	    self#test_list_NH flags fn
	  else
	    [] in
	let edfs =
	  if not(list_isect_empty tests [`E;`D;`F;`S]) then
	    self#test_list_EDFS flags fn
	  else
	    [] in
	let rwx =
	  if not(list_isect_empty tests [`R;`W;`X]) then
	    self#test_list_RWX flags fn
	  else
	    [] in
	List.map
	  (fun t ->
	     match t with
	       | `N | `H -> List.mem t nh
	       | `E | `D | `F | `S -> List.mem t edfs
	       | `R | `W | `X -> List.mem t rwx
	  )
	  tests

      method remove flags filename =
	let fn = real_root ^ check_and_norm_path filename in
	if List.mem `Recursive flags then (
	  try
	    self#rm_r_safe fn
	  with Unavailable ->
	    self#rm_r_trad fn
	)
	else
	  Unix.unlink fn

      (* A rename race: while the recursive removal progresses, a second
	 process renames the directory. The removal function suddenly
	 does not find the directory anymore. Even worse, the second
	 process could move a different directory into the place of the
	 old directory being deleted. In this case, the wrong data would
	 be deleted.

	 We can avoid this in the style of rm_r_safe, or by chdir-ing
	 into the directory hierarchy. The latter is incompatible with
	 multi-threading, so we don't do it here.
       *)

      method private rm_r_trad fn =
	(* "traditional" implemenation w/o protection against rename races *)
	let is_dir fn =
	  try (Unix.stat fn).Unix.st_kind = Unix.S_DIR
	  with _ -> false in
	let rec recurse fn =
	  if is_dir fn then (
	    let files = readdir (Unix.opendir fn) in
	    List.iter
	      (fun file ->
		 if file <> "." && file <> ".." then (
		   recurse (fn ^ "/" ^ file)
		 )
	      )
	      files;
	    Unix.rmdir fn;
	  )
	  else
	    Unix.unlink fn in
	recurse fn

      method private rm_r_safe fn =
	(* safer implemention using openat and fdopendir *)
	let rec rm_dir_entries fd =
	  let files = readdir (Netsys_posix.fdopendir (Unix.dup fd)) in
	  List.iter
	    (fun file ->
	       if file <> "." && file <> ".." then
		 rm_dir_or_file fd file
	    )
	    files
	and rm_dir_or_file fd file =
	  let file_fd = Netsys_posix.openat fd file [Unix.O_RDONLY] 0 in
	  let file_is_dir =
	    try (Unix.fstat file_fd).Unix.st_kind = Unix.S_DIR
	    with _ -> false in
	  if file_is_dir then (
	    ( try rm_dir_entries file_fd
	      with error -> Unix.close file_fd; raise error
	    );
	    Unix.close file_fd;
	    Netsys_posix.unlinkat fd file [Netsys_posix.AT_REMOVEDIR]
	  )
	  else (
	    Unix.close file_fd;
	    Netsys_posix.unlinkat fd file []
	  ) in
	let test_availability() =
	  if not (Netsys_posix.have_at()) then raise Unavailable;
	  try
	    let dir = 
	      Netsys_posix.fdopendir(Unix.openfile "." [Unix.O_RDONLY] 0) in
	    Unix.closedir dir
	  with _ -> raise Unavailable in
	test_availability();
	rm_dir_or_file Netsys_posix.at_fdcwd fn

      method rename flags oldname newname =
	let oldfn = real_root ^ check_and_norm_path oldname in
	let newfn = real_root ^ check_and_norm_path newname in
	Unix.rename oldfn newfn

      method symlink flags oldpath newpath =
	let oldfn = real_root ^ check_and_norm_path oldpath in
	let newfn = real_root ^ check_and_norm_path newpath in
	Unix.symlink oldfn newfn

      method readdir flags filename =
	let fn = real_root ^ check_and_norm_path filename in
	readdir (Unix.opendir fn)

      method readlink flags filename =
	let fn = real_root ^ check_and_norm_path filename in
	Unix.readlink fn

      method mkdir flags filename =
	if List.mem `Path flags then
	  self#mkdir_p filename
	else (
	  let fn = real_root ^ check_and_norm_path filename in
	  try
	    Unix.mkdir fn 0o777
	  with
	    | Unix.Unix_error(Unix.EEXIST,_,_) when List.mem `Nonexcl flags ->
		()
	)
	      
      method private mkdir_p filename =
	let rec traverse curdir todo =
	  match todo with
	    | [] -> ()
	    | d :: todo' ->
		let curdir' = curdir @ [d] in
		let p = String.concat "/" curdir' in
		let fn = real_root ^ p in
		( try Unix.mkdir fn 0o777 
		  with Unix.Unix_error(Unix.EEXIST,_,_) -> ()
		);
		traverse curdir' todo' in
	let fn1 = check_and_norm_path filename in
	let l = Netstring_pcre.split_delim slash_re fn1 in
	traverse [List.hd l] (List.tl l)

      method rmdir flags filename =
	let fn = real_root ^ check_and_norm_path filename in
	Unix.rmdir fn

      method copy flags srcfilename destfilename =
	copy_prim ~streaming:false self srcfilename self destfilename
    end
  )


let convert_path ?subst oldfs newfs oldpath =
  match oldfs#path_encoding, newfs#path_encoding with
    | Some oldenc, Some newenc ->
	Netconversion.convert ?subst ~in_enc:oldenc ~out_enc:newenc oldpath
    | _ ->
	oldpath


let copy ?(replace=false) ?(streaming=false)
         (orig_fs:stream_fs) orig_name dest_fs dest_name =
  if replace then
    dest_fs # remove [] dest_name;
  try
    if orig_fs = dest_fs then
      orig_fs # copy [] orig_name dest_name
    else
      raise(Unix.Unix_error(Unix.ENOSYS,"",""))
  with
    | Unix.Unix_error(Unix.ENOSYS,_,_) ->
	copy_prim ~streaming orig_fs orig_name dest_fs dest_name

type file_kind = [ `Regular | `Directory | `Symlink | `Other | `None ]


let iter ~pre ?(post=fun _ -> ()) (fs:stream_fs) start =
  let rec iter_members dir rdir =
    let files = fs # readdir [] dir in
    List.iter
      (fun file ->
	 if file <> "." && file <> ".." then (
	   let absfile = dir ^ "/" ^ file in
	   let relfile = if rdir="" then file else rdir ^ "/" ^ file in
	   let l0 = fs#test_list [] absfile [`D; `F; `E] in
	   let l1 = fs#test_list [`Link] absfile [`D; `F; `H] in
	   let (is_dir0, is_reg0, is_existing) =
	     match l0 with 
	       | [is_dir; is_reg; is_ex] -> (is_dir, is_reg, is_ex)
	       | _ -> assert false in
	   let (is_dir1, is_reg1, is_link) =
	     match l1 with 
	       | [is_dir; is_reg; is_link] -> (is_dir, is_reg, is_link)
	       | _ -> assert false in
	   if is_dir1 then (
	     pre relfile `Directory `Directory;
	     iter_members absfile relfile;
	     post relfile
	   )
	   else (
	     let t0 = 
	       if is_reg0 then `Regular 
	       else if is_dir0 then `Directory 
	       else if is_existing then `Other
	       else `None in
	     let t1 = 
	       if is_reg1 then `Regular 
	       else if is_dir1 then `Directory 
	       else if is_link then `Symlink
	       else  `Other in
	     pre relfile t0 t1
	   )
	 )
      )
      files in
  iter_members start ""


let copy_into ?(replace=false) ?subst ?streaming
              (orig_fs:stream_fs) orig_name dest_fs dest_name =
  let orig_base = Filename.basename orig_name in
  let dest_start = 
    dest_name ^ "/" ^ convert_path ?subst orig_fs dest_fs orig_base in
  if not(dest_fs # test [] dest_name `D) then
    raise(Unix.Unix_error
	    (Unix.ENOENT,
	     "Netfs.copy_into: destination directory does not exist",
	     dest_name));
  if orig_fs # test [] orig_name `D then (
    if replace then
      dest_fs # remove [ `Recursive ] dest_start;
    dest_fs # mkdir [ `Nonexcl ] dest_start;
    iter
      ~pre:(fun rpath typ link_typ ->
	      let dest_rpath =
		convert_path ?subst orig_fs dest_fs rpath in
	      match link_typ with
		| `Regular ->
		    copy 
		      ?streaming
		      orig_fs (orig_name ^ "/" ^ rpath) 
		      dest_fs (dest_start ^ "/" ^ dest_rpath)
		| `Directory ->
		    dest_fs # mkdir
		      [ `Nonexcl ] (dest_start ^ "/" ^ dest_rpath)
		| `Symlink ->
		    dest_fs # symlink
		      []
		      (orig_fs # readlink [] (orig_name ^ "/" ^ rpath))
		      (dest_start ^ "/" ^ dest_rpath)
		| `Other ->
		    ()
	   )
      orig_fs
      orig_name
  )
  else
    copy ~replace ?streaming orig_fs orig_name dest_fs dest_start
