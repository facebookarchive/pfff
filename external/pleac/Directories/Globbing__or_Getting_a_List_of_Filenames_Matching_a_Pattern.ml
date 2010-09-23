(* ********************************************************************** *)
(* Globbing, or Getting a List of Filenames Matching a Pattern *)
(* ********************************************************************** *)
let pleac_Globbing__or_Getting_a_List_of_Filenames_Matching_a_Pattern () = 
  (* See recipe 6.9 for a more powerful globber. *)
  #load "str.cma";;
  
  (* OCaml does not come with a globbing function. As a workaround, the
     following function builds a regular expression from a glob pattern.
     Only the '*' and '?' wildcards are recognized. *)
  let regexp_of_glob pat =
    Str.regexp
      (Printf.sprintf "^%s$"
         (String.concat ""
            (List.map
               (function
                  | Str.Text s -> Str.quote s
                  | Str.Delim "*" -> ".*"
                  | Str.Delim "?" -> "."
                  | Str.Delim _ -> assert false)
               (Str.full_split (Str.regexp "[*?]") pat))))
  
  (* Now we can build a very basic globber. Only the filename part will
     be used in the glob pattern, so directory wildcards will break in
     this simple example. *)
  let glob pat =
    let basedir = Filename.dirname pat in
    let files = Sys.readdir basedir in
    let regexp = regexp_of_glob (Filename.basename pat) in
    List.map
      (Filename.concat basedir)
      (List.filter
         (fun file -> Str.string_match regexp file 0)
         (Array.to_list files))
  
  (* Find all data files in the pleac directory. *)
  let files = glob "pleac/*.data"
  
  (*-----------------------------*)
  
  (* Find and sort directories with numeric names. *)
  let dirs =
    List.map snd                             (* extract pathnames *)
      (List.sort compare                     (* sort names numerically *)
         (List.filter                        (* path is a dir *)
            (fun (_, s) -> Sys.is_directory s)
            (List.map                        (* form (name, path) *)
               (fun s -> (int_of_string s, Filename.concat path s))
               (List.filter                  (* just numerics *)
                  (fun s ->
                     try ignore (int_of_string s); true
                     with _ -> false)
                  (Array.to_list
                     (Sys.readdir path)))))) (* all files *)
  

