(* ********************************************************************** *)
(* Creating Arrays of Scalar References *)
(* ********************************************************************** *)
let pleac_Creating_Arrays_of_Scalar_References () = 
  (* Create a couple of integer references *)
  let a = ref 0
  let b = ref 0
  
  (* Create an array of the references *)
  let array_of_refs = [| a; b |]
  
  let () =
    (* Set the value of an element *)
    array_of_refs.(1) := 12;              (* b := 12 *)
  
    (* Note that this is *not* the same as array mutation! If we were to do:
         array_of_refs.(1) <- ref 12
       (or drop the refs altogether) then we would no longer be aliasing "b".
    *)
  
    (* Get the value of an element *)
    Printf.printf "%d %d\n" !(array_of_refs.(1)) !b
  
  let () =
    let (a, b, c, d) = (ref 1, ref 2, ref 3, ref 4) in (* initialize *)
    let array = [| a; b; c; d |] in                    (* refs to each value *)
  
    array.(2) := !(array.(2)) + 9;        (* !c is now 12 *)
  
    let tmp = array.(Array.length array - 1) in
    tmp := !tmp * 5;                      (* !d is now 20 *)
  

