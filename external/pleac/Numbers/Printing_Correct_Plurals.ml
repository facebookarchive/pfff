(* ********************************************************************** *)
(* Printing Correct Plurals *)
(* ********************************************************************** *)
let pleac_Printing_Correct_Plurals () = 
  (* Hardcoded examples can be done as follows: *)
  Printf.printf "It took %d hour%s\n" n (if n <> 1 then "s" else "");;
  Printf.printf "It took %d centur%s\n" n (if n <> 1 then "ies" else "y");;
  
  (* For a more general solution *)
  (* First define the rules *)
  (* Note: the OS needs to support dynamic loading of C libraries for this *)
  #load "str.cma";;
  
  let rules = 
    List.map (fun x -> (Str.regexp (fst x)),(snd x))
      ["\\([psc]h\\)$\\|z$","\\0es";
       "\\(ff\\)$\\|\\(ey\\)$","\\0s";
       "f$","ves";
       "y$","ies";
       "ix$","ices";
       "ius$","ii";
       "[sx]$","\\0es";
       "non","na"];;
  
  let f w x =
    ignore(Str.search_forward (fst x) w 0); 
    Str.replace_first (fst x) (snd x) w;;
  
  let rec exn_map ex fn1 fn2 l =
    match l with
      [] -> fn2
    | h::t -> try (fn1 h) with ex -> exn_map ex fn1 fn2 t;;
  
  let pluralize x = (* "wish" in *)
    exn_map Not_found (f x) (x ^ "s") rules;;
    
  (* Note: This next example doesn't work on the odd cases *)
  let nouns = ["fish"; "fly"; "ox"; "species"; "genus"; "phylum"; "cherub";
               "radius"; "jockey"; "index"; "matrix"; "mythos"; "phenomenon";
               "formula"];;
  List.iter (fun x -> printf "One %s, two %s\n" x (pluralize x)) nouns;;
    
  

