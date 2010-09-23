(* ********************************************************************** *)
(* Merging Hashes *)
(* ********************************************************************** *)
let pleac_Merging_Hashes () = 
  (*-----------------------------*)
  (* definition of merge function on hashes: *)
  let hashtbl_merge h1 h2 = assoc_list2hashtbl (hashtbl2assoc_list h1 @ hashtbl2assoc_list h2)
  
  (* usage: *)
  let merged = hashtbl_merge a b;;
  (*-----------------------------*)
  let merged = Hashtbl.create 0 in
  List.iter
    (Hashtbl.iter (fun k v -> Hashtbl.add merged k v))
    [a;b]
  ;;
  (*-----------------------------*)
  let drink_color = assoc_list2hashtbl
      ["Galliano", "yellow";
       "Mai Tai", "blue"]
  ;;
  
  let ingested_color = hashtbl_merge drink_color food_color;;
  (*-----------------------------*)
  let substance_color = Hashtbl.create 0 in
  List.iter
    (Hashtbl.iter (fun k v -> Hashtbl.add merged k v))
    [food_color; drink_color]
  ;;
  

