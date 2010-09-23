(* ********************************************************************** *)
(* Finding Elements in One Array but Not Another *)
(* ********************************************************************** *)
let pleac_Finding_Elements_in_One_Array_but_Not_Another () = 
  
  (* using hashtables, like the cookbook *)
  let arrayDiff a b = 
    let seen = Hashtbl.create 17 
    and l = ref [] in
    Array.iter (fun x -> Hashtbl.add seen x 1) b;
    Array.iter (fun x -> if not (Hashtbl.mem seen x) then l := x::!l) a;
    Array.of_list !l;;
  
  

