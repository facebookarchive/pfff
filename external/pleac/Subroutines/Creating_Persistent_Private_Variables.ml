(* ********************************************************************** *)
(* Creating Persistent Private Variables *)
(* ********************************************************************** *)
let pleac_Creating_Persistent_Private_Variables () = 
  
  let mysub =
    let variable = ... in
    fun args -> ... ;;
  
  (* To write a counter *)
  let next_counter = 
    let counter = ref 0 in
    fun () -> 
      incr counter; 
      !counter;;
  
  let next_counter,prev_counter = 
    let counter = ref 42 in
    (fun () -> incr counter; !counter),
    (fun () -> decr counter; !counter);;
  

