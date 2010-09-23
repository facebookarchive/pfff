(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  (*-----------------------------*)
  (* build an hash table element by element *)
  let age = Hashtbl.create 3 ;;  (* 3 is the supposed average size for the
                                    hash table *)
  Hashtbl.replace age "Nat" 24 ;
  Hashtbl.replace age "Jules" 25 ;
  Hashtbl.replace age "Josh" 17 ;;
  (*-----------------------------*)
  let assoc_list2hashtbl assoc_list = 
    let h = Hashtbl.create 0 in
    List.iter (fun (k,v) -> Hashtbl.replace h k v) assoc_list ;
    h
  
  let food_color = assoc_list2hashtbl 
      [ "Apple", "red" ; 
        "Banana", "yellow" ; 
        "Lemon", "yellow" ; 
        "Carrot", "orange" ;
      ] ;;
  (*-----------------------------*)
  

