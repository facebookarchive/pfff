(* ********************************************************************** *)
(* Finding the First List Element That Passes a Test *)
(* ********************************************************************** *)
let pleac_Finding_the_First_List_Element_That_Passes_a_Test () = 
  
  (* To find the first element in a list that satisfies some predicate, just use
   * the List.find function to return an 'a option *)
  
  match
    (try Some (List.find (fun x -> x > 10) l)
    with Not_found -> None)
  with
      None -> (* unfound *)
    | Some x -> (* Do something with x *);;
  
  (* Note that this is a very general form, and can be shortened in some cases *)
  let pf l =
    try 
      printf "hah! Found %d!\n" (List.find (fun x -> x > 10) l)
    with 
      Not_found -> "Sorry charly!\n";;
  
  (*
  # pf [1;2;3;4;5;6];;
  Sorry charly!
  
  # pf [1;2;3;50;100];;
  Hah!  Found 50!
  *)
  
  (* To return the index of a matching element in an array, we can use exceptions
   * to short circuit the search *)
  
  exception Found of int;;
  
  let findi pred arr = 
    Array.iteri (fun i x -> if pred x then raise (Found i)) arr;
    raise Not_found;;
  
  let f arr = 
  try
    findi (fun x -> x > 10) arr
  with
    Found i -> printf "element %d is a big element - %d\n" i arr.(i)
  | Not_found -> printf "Only small values here!\n";;
  
  (*
  # f [|1; 2; 3; 4; 5; 6|];;
  Only small values here!
  
  # f [|1; 2; 3; 4; 5; 60; 8; 9; 100|];;
  element 5 is a big element - 60
  *)
  
  let highest_engineer =
    List.find (fun x -> x#category = "engineer") employees in
    printf "Highest paid engineer is: %s\n" highest_engineer#name;;
  
  

