(* ********************************************************************** *)
(* Passing Arrays and Hashes by Reference *)
(* ********************************************************************** *)
let pleac_Passing_Arrays_and_Hashes_by_Reference () = 
  (* Because all OCaml variables represent pointers to their data, all function
   * arguments are implicitly passed by reference *)
  
  array_diff array1 array2;;
  
  let a = [| 1; 2 |];;
  let b = [| 5; 8 |];;
  let add_vec_pair x y =
    Array.init (Array.length x) (fun i -> x.(i) + y.(i));;
  
  (*
  # let c = add_vec_pair a b;;
  val c : int array = [|6; 10|]
  *)
  

