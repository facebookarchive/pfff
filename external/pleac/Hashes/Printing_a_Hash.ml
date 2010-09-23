(* ********************************************************************** *)
(* Printing a Hash *)
(* ********************************************************************** *)
let pleac_Printing_a_Hash () = 
  (*-----------------------------*)
  (* note that OCaml does not have a native polymorphic print function, so
  examples in this section work for hashes that map string keys to string
  values *)
  Hashtbl.iter (printf "%s => %s\n") hash ;
  (*-----------------------------*)
  
  (* map in ocaml maps a function on a list, rather that evaluate an
  expression in turn on a list as Perl does *)
  List.iter
    (fun key ->
      printf "%s => %s\n" key (Hashtbl.find hash key)
    )
    (hashtbl_keys hash) ;
  (*-----------------------------*)
  
  (* build a list from an hash table, note that this is possibile only if
  the type of key and value are the same *)
  let hashtbl2list hash =
    Hashtbl.fold
      (fun key value init -> key :: value :: init)
      hash
      []
  ;;
  List.iter (printf "%s ") (hashtbl2list hash) ;
  (* or *)
  print_endline (String.concat " " (hashtbl2list hash)) ;
  

