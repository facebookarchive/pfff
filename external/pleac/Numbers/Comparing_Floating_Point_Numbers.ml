(* ********************************************************************** *)
(* Comparing Floating-Point Numbers *)
(* ********************************************************************** *)
let pleac_Comparing_Floating_Point_Numbers () = 
  (*-----------------------------*)
  (* equalStr num1 num2 accuracy returns true if num1 and num2 
     are equal to accuracy decimal places *)
  
  (* done by converting to strings, a la the Perl example *)   
  let equalStr num1 num2 accuracy =
    let p x = sprintf "%.*f" accuracy x in
    (p num1) = (p num2)
  
  (* Done in a more or less sane way, i.e. treating them as numbers *)
  let equal num1 num2 accuracy =
    let chop x = floor (x *. (10. ** (float accuracy))) in
    (chop num1) = (chop num2);;
  
  (*-----------------------------*)
  let wage = 536;;
  let week = 40 * wage;;
  Printf.printf "One week's wage is %.2f\n" ((float week) /. 100.);;
  (*-----------------------------*)
  

