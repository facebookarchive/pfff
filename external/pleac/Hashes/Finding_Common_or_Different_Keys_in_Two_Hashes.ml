(* ********************************************************************** *)
(* Finding Common or Different Keys in Two Hashes *)
(* ********************************************************************** *)
let pleac_Finding_Common_or_Different_Keys_in_Two_Hashes () = 
  (*-----------------------------*)
  let common =
    List.filter
      (fun key -> Hashtbl.mem hash2 key)
      (hashtbl_keys hash1)
  ;;
  (* common now contains commne keys, note that a key may appear multiple
  times in this list due tu multiple bindings allowed in Hashtbl
  implementation *)
  
  let this_not_that =
    List.filter
      (fun key -> not (Hashtbl.mem hash2 key))
      (hashtbl_keys hash1)
  ;;
  (*-----------------------------*)
  let citrus_color = assoc_list2hashtbl
                        ["Lemon", "yellow";
                         "Orange", "orange";
                         "Lime", "green"]
  in
  let non_citrus = Hashtbl.create 3 in
  List.filter
    (fun key -> not (Hashtbl.mem citrus_color key))
    (hashtbl_keys food_color)
  ;;
  
  (*-----------------------------*)

