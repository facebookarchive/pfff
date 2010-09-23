(* ********************************************************************** *)
(* Reversing an Array *)
(* ********************************************************************** *)
let pleac_Reversing_an_Array () = 
  
  (* To reverse a list, use List.rev *)
  let reversed = List.rev l;;
  
  (* For an array, it is probably easiest to use Array.init *)
  
  let revArray a = 
    let len = Array.length a - 1 in
    Array.init len+1 (fun i -> a.(len - i);;
  
  let reversed = revArray a;;
  
  (* Or one can use a for loop *)
  for i = Array.length a - 1 downto 0 do
    (* Do something to a.(i) *)
  done;;
  

