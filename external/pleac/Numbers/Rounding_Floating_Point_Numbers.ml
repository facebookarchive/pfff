(* ********************************************************************** *)
(* Rounding Floating-Point Numbers *)
(* ********************************************************************** *)
let pleac_Rounding_Floating_Point_Numbers () = 
  (*-----------------------------*)
  let rounded digits fl = float_of_string (sprintf "%.*f" digits fl);;
  (*-----------------------------*)
  let a = 0.255;;
  let b = float_of_string (sprintf "%.2f" a);;
  let c = rounded 2 a;;
  printf "Unrounded %f\nRounded %f\nOther rounded %f\n" a b c;;
  printf "Unrounded %f\nRounded %.2f\nOther rounded %f\n" a c (rounded 2 a);;
  
  (*
   * Unrounded 0.255000
   * Rounded 0.260000
   * Other rounded 0.260000
   * Unrounded 0.255000
   * Rounded 0.26
   * Other rounded 0.260000
   *)
  
  (*-----------------------------*)
  (* To "round" to the nearest integer, use ceil, floor, or truncate.  Note that
  truncate converts the float to an integer, so a conversion back to a float is
  necessary *)
  let fs = [3.3; 3.5; 3.7; -. 3.3];;
  printf "number\tint\tfloor\tceil\n";
  List.iter 
    (fun x -> printf "%.1f\t%.1f\t%.1f\t%.1f\n" x (float (truncate x)) (floor x) (ceil x)) 
    fs;;
  
  (*
   * number	int	floor	ceil
   * 3.3	3.0	3.0	4.0
   * 3.5	3.0	3.0	4.0
   * 3.7	3.0	3.0	4.0
   * -3.3	-3.0	-4.0	-3.0
   *) 
    
  (* Or if you really want an integer in column 2 *)
  printf "number\tint\tfloor\tceil\n";
  List.iter 
    (fun x -> printf "%.1f\t%d\t%.1f\t%.1f\n" x (truncate x) (floor x) (ceil x)) 
    fs;;
  
  (* 
   * number	int	floor	ceil
   * 3.3	3	3.0	4.0
   * 3.5	3	3.0	4.0
   * 3.7	3	3.0	4.0
   * -3.3	-3	-4.0	-3.0
   *) 
  

