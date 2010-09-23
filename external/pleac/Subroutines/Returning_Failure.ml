(* ********************************************************************** *)
(* Returning Failure *)
(* ********************************************************************** *)
let pleac_Returning_Failure () = 
  (* Use an appropriate exception *)
  
  let failing_routine () =
    ...
    raise Failure "Bad things happened...";;
  
  try failing_routine () with
    Failure s -> printf "failing_routine failed because: %s\n" s;;
  

