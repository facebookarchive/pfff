(* ********************************************************************** *)
(* Program: symirror *)
(* ********************************************************************** *)
let pleac_Program__symirror () = 
  #!/usr/bin/ocaml
  (* symirror - build spectral forest of symlinks *)
  #load "unix.cma";;
  
  open Printf
  
  let die msg = prerr_endline msg; exit 1
  
  let () =
    if Array.length Sys.argv <> 3
    then die (sprintf "usage: %s realdir mirrordir" Sys.argv.(0))
  
  let srcdir, dstdir = Sys.argv.(1), Sys.argv.(2)
  let cwd = Unix.getcwd ()
  
  let fix_relative path =
    if Filename.is_relative path
    then Filename.concat cwd path
    else path
  
  let is_dir dir =
    try Some (Sys.is_directory dir)
    with Sys_error _ -> None
  
  let () =
    match (is_dir srcdir, is_dir dstdir) with
      | (None, _) | (Some false, _) ->
          die (sprintf "%s: %s is not a directory" Sys.argv.(0) srcdir)
      | (_, Some false) ->
          die (sprintf "%s: %s is not a directory" Sys.argv.(0) dstdir)
      | (_, None) ->
          Unix.mkdir dstdir 0o7777        (* be forgiving *)
      | (Some _, Some _) ->
          ()                              (* cool *)
  
  (* fix relative paths *)
  let srcdir, dstdir = fix_relative srcdir, fix_relative dstdir
  
  let rec find f roots =
    Array.iter
      (fun root ->
         f root;
         match Unix.lstat root with
           | {Unix.st_kind=Unix.S_DIR} ->
               find f (Array.map
                         (Filename.concat root)
                         (Sys.readdir root))
           | _ -> ())
      roots
  
  let wanted name =
    if name <> Filename.current_dir_name
    then
      let {Unix.st_dev=dev; st_ino=ino; st_kind=kind; st_perm=perm} =
        Unix.lstat name in
      (* preserve directory permissions *)
      let perm = perm land 0o7777 in
      (* correct name *)
      let name =
        if String.length name > 2 && String.sub name 0 2 = "./"
        then String.sub name 2 (String.length name - 2)
        else name in
      if kind = Unix.S_DIR
      then
        (* make a real directory *)
        Unix.mkdir (Filename.concat dstdir name) perm
      else
        (* shadow everything else *)
        Unix.symlink
          (Filename.concat srcdir name)
          (Filename.concat dstdir name)
  
  let () =
    Unix.chdir srcdir;
    find wanted [|"."|]
  

