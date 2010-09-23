(* ********************************************************************** *)
(* Retrieving from a Hash in Insertion Order *)
(* ********************************************************************** *)
let pleac_Retrieving_from_a_Hash_in_Insertion_Order () = 
  (*-----------------------------*)
  (* In OCaml one usually use association lists which really is a list of
  (key,value). Note that insertion and lookup is O(n) (!!!) *)
  
  (* initialization *)
  let empty_food_color = []
  let food_color = 
      [ "Banana", "Yellow" ; 
        "Apple", "Green" ; 
        "Lemon", "Yellow" ; 
      ]
  (* adding *)
  let food_color' = food_color @ [ "Carrot", "orange" ]
  ;;
  (* output entries in insertion order *)
  print_endline "In insertion order, the foods are:";
  List.iter (printf "%s is colored %s.\n") food_color;
  (*
  > Banana is colored Yellow.
  > Apple is colored Green.
  > Lemon is colored Yellow.
  *)
  (* is it a key? *)
  let has_food food = mem_assoc food food_color
  (* remove a key *)
  let remove_food food = remove_assoc food food_color
  (* searching *)
  let what_color food =
    try
      let color = assoc food food_color in
      printf "%s is colored %s.\n" food color
    with Not_found -> printf "i don't know the color of %s\n" food
  ;;

