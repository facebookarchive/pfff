(* ********************************************************************** *)
(* Returning More Than One Array or Hash *)
(* ********************************************************************** *)
let pleac_Returning_More_Than_One_Array_or_Hash () = 
  (* Just stick all of the values in a tuple and return it *)
  let somefunc () =
    let arr = ... in
    let hash = ... in
      ...
      (arr,hash);;
  
  let a,h = somefunc ();;
  

