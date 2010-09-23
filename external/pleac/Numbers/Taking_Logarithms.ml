(* ********************************************************************** *)
(* Taking Logarithms *)
(* ********************************************************************** *)
let pleac_Taking_Logarithms () = 
  
  (* to take a natural log, use the log function *)
  let log_e = log 100.;;
  
  (* to take a log to base 10, use the log10 function *)
  let log_10 = log10 100.;;
  
  (* to take a log to an arbitrary base, use traditional identities *)
  let logB base x = (log x) /. (log base);;
  
  

