(* ********************************************************************** *)
(* Inverting a Hash *)
(* ********************************************************************** *)
let pleac_Inverting_a_Hash () = 
  (*-----------------------------*)
  
  open Hashtbl
  
  (* size of an hash, i.e. number of bindings *)
  let hashtbl_size h = List.length (hashtbl_keys h);;
  
  (* in OCaml does not exists a builtin function like "reverse", here is
  an equivalent one: *)
  let hashtbl_reverse h =
    assoc_list2hashtbl (List.combine (hashtbl_values h) (hashtbl_keys h))
  (* or *)
  let hashtbl_reverse h =
    assoc_list2hashtbl (List.map (fun (a,b) -> (b,a)) (hashtbl2assoc_list h))
  ;;
  (* or *)
  let hashtbl_reverse_multi h =
    let newhash = Hashtbl.create (hashtbl_size h) in
    List.iter
      (fun v -> add newhash (find h v) v)
      (hashtbl_keys h);
    newhash
  (* note that the last  implementation maintain also multiple binding for the
  same key, see Hashtbl.add in the standard OCaml library for more info *)
  
  (*-----------------------------*)
  (* example of hashtbl_reverse *)
  
  let reverse = hashtbl_reverse lookup;;
  (*-----------------------------*)
  let surname = assoc_list2hashtbl ["Mickey", "Mantle"; "Babe", "Ruth"] in
  let firstname = hashtbl_reverse surname in
  print_endline (Hashtbl.find firstname "Mantle");;
  (*
  > Mickey
  *)
  
  (*-----------------------------*)
  (* foodfind - find match for food or color *)
  
  let given = Sys.argv.(1) in
  let color = assoc_list2hashtbl
    ["Apple", "red";
     "Banana", "yellow";
     "Lemon", "yellow";
     "Carrot", "orange"] in
  let food = hashtbl_reverse color in
  (try
    printf "%s is a food with color %s.\n" given (Hashtbl.find color given);
  with Not_found -> ());
  (try
    printf "%s is a food with color %s.\n" (Hashtbl.find food given) given
  with Not_found -> ())
  ;;
  (*-----------------------------*)
  (* food_color defined as previous *)
  
  let foods_with_color = hashtbl_reverse food_color in
  List.iter (printf "%s ") (Hashtbl.find_all foods_with_color "yellow");
  print_endline "were yellow foods."
  ;;
  (*-----------------------------*)
  

