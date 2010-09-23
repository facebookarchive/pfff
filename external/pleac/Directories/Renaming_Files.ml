(* ********************************************************************** *)
(* Renaming Files *)
(* ********************************************************************** *)
let pleac_Renaming_Files () = 
  #load "unix.cma";;
  let () = List.iter
    (fun file ->
       let newname = file in
       (* change newname *)
       Unix.rename file newname)
    names
  
  (* rename - Larry's filename fixer *)
  #load "unix.cma";;
  #directory "+pcre";;
  #load "pcre.cma";;
  let () =
    match Array.to_list Sys.argv with
      | prog :: pat :: templ :: files ->
          let replace = Pcre.replace ~pat ~templ in
          List.iter
            (fun file ->
               let file' = replace file in
               Unix.rename file file')
            files
      | _ -> prerr_endline "Usage: rename pattern replacment [files]"
  
  (*
    % rename '\.orig$' '' *.orig
    % rename '$' '.bad' *.f
    % rename '([^/]+)~$' '.#$1' /tmp/*~
    % find /tmp -name '*~' -exec rename '([^/]+)~$' '.#$1' {} \;
  *)
  

