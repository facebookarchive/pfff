(* ********************************************************************** *)
(* Sorting a Hash *)
(* ********************************************************************** *)
let pleac_Sorting_a_Hash () = 
  (*-----------------------------*)
  
  (* you may define your own compare function to be used in sorting *)
  let keys = List.sort compare_function (hashtbl_keys hash) in
  List.iter
    (fun key ->
      let value = Hashtbl.find hash key in
      (* do something with key and value *)
      ()
    )
    keys ;
  (* or use this one if you want to compare not only on keys *)
  Hashtbl.iter
    (fun (key, value) ->
      (* do something with key and value *)
      ()
    ) (List.sort compare_function (hashtbl2assoc_list hash)) ;
  (*-----------------------------*)
  List.iter
    (fun food ->
      printf "%s is %s.\n" food (Hashtbl.find food_color food)
    )
    (List.sort (hashtbl_keys food_color))
  ;;
  (*-----------------------------*)
  (* examples of "compare_function": *)
  
  (* alphabetical sort on the hash value *)
  let compare_function (_,color1) (_,color2) = compare color1 color2
  
  (* length sort on the hash value *)
  let compare_function (_,color1) (_,color2) = compare (String.length color1) (String.length color2)
  
  (*-----------------------------*)
  

