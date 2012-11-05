(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)


open ExtList
open JBasics
open JClassLow

let sep =
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> ":"
    | "Win32" -> ";"
    | _ -> assert false

let replace_dot s =
  let s = String.copy s in
    for i = 0 to String.length s - 1 do
      if s.[i] = '.' then s.[i] <- '/'
    done;
    s

(* [mkdir -p] *)
let rec mkdir d perms =
  if not (Sys.file_exists d)
  then (
    mkdir (Filename.dirname d) perms;
    try
      (* there may be an error as the recursive call may have already
         build d if d was equal to "foo/bar/." *)
      Unix.mkdir d perms
    with Unix.Unix_error _ -> ()
  )

let is_dir d =
  try
    (Unix.stat d).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false

let is_file f =
  try
    (Unix.stat f).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false


(* We should catch only the exceptions Unix_error _ and End_of_file
   that we raised. *)

type cp_unit = [`dir of string | `jar of Zip.in_file]
type class_path = cp_unit list

let open_path s =
  if is_dir s
  then Some (`dir s)
  else
    if (Filename.check_suffix s ".jar" or Filename.check_suffix s ".zip") && is_file s
    then Some (`jar (Zip.open_in s))
    else None

type directories = string list

let make_directories dirs =
  match ExtString.String.nsplit dirs sep with
    | [] -> [Filename.current_dir_name]
    | cp ->
	List.filter is_dir cp

let class_path cp =
  let cp_list =
    match ExtString.String.nsplit cp sep with
      | [] -> [Filename.current_dir_name]
      | cp -> cp
  in
  let cp_with_jars =
    List.concat
      (List.map
	 (fun cp_item ->
	      if Sys.is_directory cp_item
	      then
		let files =
		  List.filter
		    (fun file -> Filename.check_suffix file ".jar" or Filename.check_suffix file ".zip")
		    (Array.to_list (Sys.readdir cp_item))
		in cp_item::(List.map (Filename.concat cp_item) files)
	      else
		[cp_item]
	 )
	 cp_list)
  in
    List.filter_map open_path cp_with_jars

let close_class_path =
  List.iter
    (function
      | `dir _ -> ()
      | `jar jar -> Zip.close_in jar)

(* Search for class c in a given directory or jar file *)
let lookup c : cp_unit -> JClassLow.jclass =
  let c = replace_dot c ^ ".class" in
    function path ->
      try
	let ch =
	  match path with
	    | `dir d ->
		if is_file (Filename.concat d c)
		then
		  let ch = open_in_bin (Filename.concat d c) in
		    IO.input_channel ch
		else raise Not_found
	    | `jar jar ->
		let e = Zip.find_entry jar c in
		  IO.input_string (Zip.read_entry jar e)
	in
	let c = JParse.parse_class_low_level ch in
	  IO.close_in ch;
	  c
      with
	| Not_found ->
	    raise (No_class_found c)

let rec fold_directories (f: 'b -> 'a) file : 'b list -> 'a = function
  | [] -> raise (No_class_found file)
  | class_path :: q ->
      try f class_path
      with No_class_found _ ->
	fold_directories f file q

let get_class_low (class_path:class_path) cs =
  let cname = JDumpBasics.class_name cs in
    fold_directories
      (fun path -> lookup cname path)
      cname
      class_path

let get_class class_path c = JLow2High.low2high_class (get_class_low class_path c)

let write_class_low output_dir classe =
  let class_name = JDumpBasics.class_name classe.j_name in
  let c = replace_dot class_name ^ ".class" in
    (mkdir
       (Filename.concat output_dir (Filename.dirname c))
       0o755);
    let f = open_out_bin (Filename.concat output_dir c) in
    let output = IO.output_channel f in
      JUnparse.unparse_class_low_level output classe;
      IO.close_out output

let write_class output_dir classe = write_class_low output_dir (JHigh2Low.high2low classe)

let dir_sep =
  match Sys.os_type with
    | "Unix" -> "/"	
    | "Win32" -> "\\"
    | "Cygwin" -> "/"
    | _ -> assert false (* Inspirated from filename.ml in the stdlib *)

let extract_class_name_from_file file =
  let input = IO.input_channel (open_in_bin file) in
  let c = JParse.parse_class_low_level input in
    IO.close_in input;
    let cname = c.j_name in
    let package = cn_package cname in
    let path =
      let p = ExtString.String.nsplit (Filename.dirname file) dir_sep in
	match p with
	  | [] -> []
	  | hd :: tl ->
	      (* We delete the unwanted empty strings due to the
		 presence of multiple consecutive separators in
		 file. *)
	      hd :: (ExtList.List.remove_all tl "") in
    let ends_with l endl =
      let n = List.length l in
      let endn = List.length endl in
	try
	  let (hl,tl) =
	    ExtList.List.split_nth (n - endn) l in
	    if (tl = endl) then (true,hl)
	    else (false,l)
	with ExtList.List.Invalid_index _ -> (false,l) in
    let (b,l) = ends_with path package in
      if b then
	let classpath = String.concat dir_sep l in
	  (cname, classpath)
      else
	(* Should we be permissive or not ? 
	   Because in this case the java command will fail. *)
	let classpath = String.concat dir_sep l in
	  (cname, classpath)
      

(* Try to open a string as a directory and recursively applies f to
   every .class file in it. Throws ENOTDIR or ENOENT otherwise. *)
let rec apply_to_dir f s =
  let rep = Unix.opendir s in
    try
      while true do
	let s' = Unix.readdir rep in
	  if
	    s' <> "." && s' <> ".."
	  then
	    let s = Filename.concat s s' in
	      try apply_to_dir f s
	      with
		  Unix.Unix_error (Unix.ENOTDIR, _, _) ->
		    (if
		       Filename.check_suffix s ".class"
		     then
		       f s)
      done
    with
	End_of_file -> Unix.closedir rep

(* Try to interpret a string as a directory or a class name without the
   .class suffix and applies f to the relevant .class files. Throws
   No_class_found otherwise. *)
let apply_to_dir_or_class f s =
  try
    apply_to_dir f s
  with
      Unix.Unix_error ((Unix.ENOTDIR | Unix.ENOENT), _, _) ->
	let class_file = s ^ ".class" in
	  if is_file class_file
	  then
	    f (class_file)
	  else
	    raise (No_class_found s)

(* Try to open a jar or zip file, checking for the .jar or .zip
   suffix. f is applied to all .class files in the archive. other is
   applied to other files.  Throws No_class_found otherwise. *)
let apply_to_jar f other s =
  if
    (Filename.check_suffix s ".jar"
     or
     Filename.check_suffix s ".zip")
    && is_file s
  then
    let jar = Zip.open_in s in
      List.iter
	(function e ->
	   if Filename.check_suffix e.Zip.filename ".class"
	   then (
	     let input = IO.input_string (Zip.read_entry jar e) in
	     let c = JParse.parse_class_low_level input in
	       IO.close_in input;
	       f c
	   ) else other jar e)
	(Zip.entries jar);
      Zip.close_in jar
  else
    raise (No_class_found s)

(* Try to read or transform a set of classes given by a string. The
   name is interpreted (in order of priority) as:
   - a directory name
   - a class name (without extension)
   - a jar file (with the .jar suffix)
   - a zip file (with the .zip suffix).
   The resulting directory, class file, or jar (or zip) file if any, is written
   in the directory given as argument of the `transform constructor.
   Throws No_class_found otherwise.
   Throws Invalid_argument if the name is not implicit (it must be relative and
   must not start with [./] or [../]) *)
let fold_string class_path f file =
  if not (Filename.is_implicit file)
  then
    invalid_arg ("invalid class name " ^ file ^ ", must be implicit")
  else
    let c =
      if (Filename.check_suffix file ".jar" or Filename.check_suffix file ".zip")
      then file
      else replace_dot file
    in
      try
	apply_to_dir_or_class
	  (function c ->
	     let ch = open_in_bin c in
	     let input = IO.input_channel ch in
	     let classe = JParse.parse_class_low_level input in
	       IO.close_in input;
	       match f with
		 | `read f ->
		     f classe
		 | `transform (output_dir, f) ->
		     let classe = f classe in
		       write_class_low output_dir classe)
	  (Filename.concat class_path c)
      with
	  No_class_found _ ->
	    match f with
	      | `read f ->
		  apply_to_jar
		    (function classe ->
		       f classe)
		    (fun _ _ -> ())
		    (Filename.concat class_path file)
	      | `transform (output_dir, f) ->
		  mkdir
		    (Filename.concat output_dir (Filename.dirname file))
		    0o755;
		  let jar' = Zip.open_out (Filename.concat output_dir file) in
		    (try
		       apply_to_jar
			 (function classe ->
			    let classe = f classe in
			    let class_name = JDumpBasics.class_name classe.j_name in
			    let c = replace_dot class_name ^ ".class"
			    and contents =
			      let s = IO.output_string () in
				JUnparse.unparse_class_low_level s classe;
				IO.close_out s in
			      Zip.add_entry contents jar' c)
			 (fun jar e ->
			    let contents = Zip.read_entry jar e in
			      Zip.add_entry contents jar' e.Zip.filename)
			 (Filename.concat class_path file);
		     with
			 e ->
			   Zip.close_out jar';
			   Unix.unlink (Filename.concat output_dir file);
			   raise e);
		    Zip.close_out jar'

exception Encapsulate of exn

(* Applies f to a list of files, in a colon-separated list of directories. *)
let fold directories f files =
  let f = try f with Not_found as e -> raise (Encapsulate e) in
    try
      List.iter
        (function file ->
           fold_directories (fun class_path -> fold_string class_path f file) file
	     directories)
        files
    with Encapsulate e -> raise e

  

let read_low directories f accu files =
  let accu = ref accu in
    fold directories (`read (function classe -> accu := f ! accu classe)) files;
    ! accu

let read directories f accu files =
  let class_struct_err = ref false in
  let accu = ref accu in
    fold directories
      (`read
	 (function classe -> 
	    try
	      accu := f ! accu (JLow2High.low2high_class classe)
	    with Class_structure_error _ as e -> 
	      prerr_endline (Printexc.to_string e);
	      class_struct_err := true
	 ))
      files;
    if !class_struct_err then
      prerr_endline "Some class files of the archive have been rejected because the specification of a class structure is broken. You could use 'JBasics.set_permissive true' to accept anyway these classes.";
    ! accu

let transform_low directories output_dir f files =
  fold directories (`transform (output_dir, f)) files

let transform directories output_dir f files =
  fold directories
    (`transform
       (output_dir,fun c -> JHigh2Low.high2low (f (JLow2High.low2high_class c))))
    files

let is_file f =
  try
    (Unix.stat f).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false

let is_dir d =
  try
    (Unix.stat d).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error (Unix.ENOENT, _,_) -> false

let make_dir_absolute dir =
  if Filename.is_relative dir
  then Filename.concat (Unix.getcwd ()) dir
  else dir

let iter ?(debug=false) f filename =
  if is_file filename && Filename.check_suffix filename ".class" then
    begin
      let cp = class_path (Filename.dirname filename) in
      let file = Filename.chop_suffix (Filename.basename filename) ".class" in
      let _ = f (get_class cp (JBasics.make_cn file)) in
	close_class_path cp
    end
  else if is_file filename && (Filename.check_suffix filename ".jar" or Filename.check_suffix filename ".zip") then
    begin
      let cp = Filename.dirname filename in
      let filename = Filename.basename filename in
      let nb_class = ref 0 in
      let _ = read (make_directories cp) (fun _ -> incr nb_class; f) () [filename] in
	if debug then
          begin
            prerr_endline (string_of_int !nb_class^" classes")
          end;
    end
  else if is_dir filename then
    let cp = filename in
    let jar_files = ref [] in
    let dir = Unix.opendir (make_dir_absolute filename) in
    let nb_class = ref 0 in
      try
	while true do
	  let next = Unix.readdir dir in
	    if (Filename.check_suffix next ".jar" or Filename.check_suffix next ".zip") 
	    then jar_files := next :: !jar_files
	done
      with End_of_file ->
	( Unix.closedir dir;
	  let _ = read (make_directories cp) (fun _ -> incr nb_class; f) () !jar_files
	  in
	    if debug then
              begin
                print_int !nb_class;
                print_string " classes in ";
                print_int (List.length !jar_files);
                print_endline " jar (or zip) files"
              end)
  else begin
    prerr_string  filename;
    prerr_endline " is not a valid class file, nor a valid jar (or zip) file, nor a directory";
    exit 0
  end

