(* ********************************************************************** *)
(* Expanding Tildes in Filenames *)
(* ********************************************************************** *)
let pleac_Expanding_Tildes_in_Filenames () = 
  #load "str.cma";;
  #load "unix.cma";;
  
  let expanduser =
    let regexp = Str.regexp "^~\\([^/]*\\)" in
    let replace s =
      match Str.matched_group 1 s with
        | "" ->
            (try Unix.getenv "HOME"
             with Not_found ->
               (try Unix.getenv "LOGDIR"
                with Not_found ->
                  (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir))
        | user -> (Unix.getpwnam user).Unix.pw_dir in
    Str.substitute_first regexp replace
  
  (*-----------------------------*)
  
      ~user
      ~user/blah
      ~
      ~/blah
  

