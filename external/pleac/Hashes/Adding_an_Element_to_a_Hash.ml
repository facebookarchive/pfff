(* ********************************************************************** *)
(* Adding an Element to a Hash *)
(* ********************************************************************** *)
let pleac_Adding_an_Element_to_a_Hash () = 
  (*-----------------------------*)
  Hashtbl.replace tbl key value ;;
  (*-----------------------------*)
  (* food_color defined per the introduction *)
  Hashtbl.replace food_color "Raspberry" "pink" ;;
  
  
  let hashtbl_keys h = Hashtbl.fold (fun key _ l -> key :: l) h []
  let hashtbl_values h = Hashtbl.fold (fun _ value l -> value :: l) h []
  let hashtbl2assoc_list h = Hashtbl.fold (fun key value l -> (key, value) :: l) h []
  ;;
  print_string "Known_foods:\n" ;
  Hashtbl.iter (fun food _ -> print_endline food) food_color ;
  print_string "Known_foods:\n" ;
  List.iter print_endline (hashtbl_keys food_color) ;;
  (*
  > Known_foods:
  > Banana
  > Raspberry
  > Apple
  > Carrot
  > Lemon
  *)
  (*-----------------------------*)
  

