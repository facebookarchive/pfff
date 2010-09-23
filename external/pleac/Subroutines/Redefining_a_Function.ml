(* ********************************************************************** *)
(* Redefining a Function *)
(* ********************************************************************** *)
let pleac_Redefining_a_Function () = 
  
  (* If you want to redefine a function... go ahead.  Functions are first class
   * members in OCaml *)
  
  let f x y =
    x + y;;
  
  f 5 7;;
  (*  - : int = 12 *)
  
  let f x y =
    x - y;;
  
  f 5 7;;
  
  (*  - : int = -2 *)
  
  (* to do it temporarily, either save to old value and then restore it, or just
   * redefine it in the current block.  The old value will be restored when you
   * exit the scope of that block *)
  
  let g = f
  and f x y =
    x * y;;
  
  f 5 7;;
  
  (*  - : int = 35 *)
  
  let f = g;;
  
  f 5 7;;
  
  (*  - : int = -2 *)
  
  let g () = 
    let f x y =
      x / y in
    f 5 7;;
  
  g ();;
  
  (*  - : int = 0 *)
  
  f 5 7;;
  
  (*  - : int = -2 *)
  

