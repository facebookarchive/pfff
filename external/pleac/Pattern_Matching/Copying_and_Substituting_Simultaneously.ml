(* ********************************************************************** *)
(* Copying and Substituting Simultaneously *)
(* ********************************************************************** *)
let pleac_Copying_and_Substituting_Simultaneously () = 
  #load "str.cma";;
  
  (* The Str module doesn't modify strings in place; you always get
     a copy when you perform a substitution. *)
  let dst = Str.global_replace (Str.regexp "this") "that" src
  
  (* Strip to basename. *)
  let progname = Str.replace_first (Str.regexp "^.*/") "" Sys.argv.(0)
  
  (* Make All Words Title-Cased. *)
  let capword =
    Str.global_substitute
      (Str.regexp "\\b.")
      (fun s -> String.uppercase (Str.matched_string s))
      words
  
  (* /usr/man/man3/foo.1 changes to /usr/man/cat3/foo.1 *)
  let catpage =
    Str.replace_first (Str.regexp "man\\([0-9]\\)") "cat\\1" manpage
  
  (* Copy and substitute on all strings in a list. *)
  let bindirs = ["/usr/bin"; "/bin"; "/usr/local/bin"]
  let libdirs =
    List.map (fun s -> Str.replace_first (Str.regexp "bin") "lib" s)
      bindirs
  (* ["/usr/lib"; "/lib"; "/usr/local/lib"] *)
  

