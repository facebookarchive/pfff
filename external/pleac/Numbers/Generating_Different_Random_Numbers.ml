(* ********************************************************************** *)
(* Generating Different Random Numbers *)
(* ********************************************************************** *)
let pleac_Generating_Different_Random_Numbers () = 
  (* Seed the generator with an integer *)
  Random.init 5;;
  
  (* Seed the generator with an array of integers *)
  Random.full_init [| 1; 2; 178653; -62 |];;
  
  (* Automatically seed the generator in a system-dependant manner *)
  Random.self_init ();;
  

