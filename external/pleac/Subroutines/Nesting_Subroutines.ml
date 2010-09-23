(* ********************************************************************** *)
(* Nesting Subroutines *)
(* ********************************************************************** *)
let pleac_Nesting_Subroutines () = 
  
  (* Just define the inner function within the outer one *)
  let outer x =
    let x = x + 35 in
    let inner () =
      x * 19 in
    x + inner ();;
  

