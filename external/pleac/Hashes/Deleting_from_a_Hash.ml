(* ********************************************************************** *)
(* Deleting from a Hash *)
(* ********************************************************************** *)
let pleac_Deleting_from_a_Hash () = 
  (*-----------------------------*)
  (* remove $KEY and its value from %HASH *)
  Hashtbl.remove hash key ;
  (*-----------------------------*)
  (* food_color as per Introduction *)
  open Printf
  
  let print_foods () =
    printf "Keys: %s\n" (String.concat " " (hashtbl_keys food_color)) ;
    printf "Values: %s\n" (String.concat " " (hashtbl_values food_color))
  ;;
  print_string "Initially:\n";
  print_foods ();
  
  print_string "\nWith Banana deleted\n";
  Hashtbl.remove food_color "Banana";
  print_foods ()
  ;;
  (*-----------------------------*)
  Hashtbl.clear food_color ;;
  (*-----------------------------*)
  

