(* ********************************************************************** *)
(* Skipping Selected Return Values *)
(* ********************************************************************** *)
let pleac_Skipping_Selected_Return_Values () = 
  (* Use _, which matches any pattern and throws away the value it matches *)
  
  let a,_,c = func ();;
  let _,_,d = func ();;
  

