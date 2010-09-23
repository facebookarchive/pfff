(* ********************************************************************** *)
(* Doing Trigonometry in Degrees, not Radians *)
(* ********************************************************************** *)
let pleac_Doing_Trigonometry_in_Degrees__not_Radians () = 
  let pi = acos(-. 1.);;
  let degrees_of_radians r = 180. *. r /. pi;;
  let radians_of_degrees d = d *. pi /. 180.;;
  
  let sinDeg d = sin (radians_of_degrees d);;
  let cosDeg d = cos (radians_of_degrees d);;
  

