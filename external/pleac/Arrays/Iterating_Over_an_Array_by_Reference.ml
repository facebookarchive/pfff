(* ********************************************************************** *)
(* Iterating Over an Array by Reference *)
(* ********************************************************************** *)
let pleac_Iterating_Over_an_Array_by_Reference () = 
  
  (* iterate over elements of array in arrayref *)
  
  Array.iter (fun x -> (* do something with x *)) !arrayref;;
  
  for i = 0 to Array.length !arrayref - 1 do
    (* do something with !arrayref.(i) *)
  done
  
  let fruits = [| "Apple"; "Blackberry" |];;
  let fruit_ref = ref fruits;;
  Array.iter (printf "%s tastes good in a pie.\n") !fruit_ref;;
  
  for i = 0 to  Array.length !fruit_ref - 1 do
    printf "%s tastes good in a pie.\n" !fruit_ref.(i)
  done;;
  
  Hashtbl.add namelist "felines" (ref rogue_cats);;
  Array.iter (printf "%s purrs hypnotically.\n") !(Hashtbl.find namelist
  "felines");;
  print_endline "--More--\nYou are controlled.";;
  
  for i=0 to Array.length !(Hashtbl.find namelist "felines") - 1 do
    printf "%s purrs hypnotically.\n" !(Hashtbl.find namelist "felines").(i)
  done;;
  

