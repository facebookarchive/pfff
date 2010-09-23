(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  
  (* A function is bound to a variable (as with everything) with the let keyword
  *)
  
  let hello () =
    incr greeted; (* global reference *)
    printf "hi there!\n";;
  
  (* Other forms for declaring a function are as follows *)
  
  let hello = 
    fun () -> 
      incr greeted; (* global reference *)
      printf "hi there!\n";;
  
  let hello = 
    function () ->
      incr greeted; (* global reference *)
      printf "hi there!\n";;
  
  (* The typical way of calling this function is *)
  
  hello ();;
  

