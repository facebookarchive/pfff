(* ********************************************************************** *)
(* Finding All Elements in an Array Matching Certain Criteria *)
(* ********************************************************************** *)
let pleac_Finding_All_Elements_in_an_Array_Matching_Certain_Criteria () = 
  
  (* to find all elements of a list that satisfy a certain predicate, just use the
   * List.find_all function *)
  
  let matching = List.find_all ( (* predicate *) l;;
  
  (* for an array, it's likely easiest to convert the original array to a list,
   * use List.find_all, and convert that list into an array *)
  let matching = 
    Array.ofList (List.find_all ( (*predicate *) ) (Array.to_list a));;
  
  (* the next example requires use of the Str library, which must be linked in.
   * In the toplevel environment use `#load "str.cma"' *)
  
  let bigs = List.find_all (fun x -> x > 1000000) nums;;
  let pigs = List.find_all (fun x -> (Hashtbl.find users x) > 1e7) 
              (Hashtbl.fold (fun k v b -> k::b) users []);;
  
  let matching = 
    List.find_all (fun x -> Str.string_match (Str.regexp "gnat") x 0) (who ());;
  
  let engineers = List.find_all (fun x -> x#position = "Engineer") employees;;
  
  let secondary_assistance = 
    List.find_all (fun x -> x#income >= 26000 && x#income < 30000) applicants;;
  

