(* ********************************************************************** *)
(* Converting DMYHMS to Epoch Seconds *)
(* ********************************************************************** *)
let pleac_Converting_DMYHMS_to_Epoch_Seconds () = 
  (*-----------------------------*)
  (*
  ** Converting DMYHMS to Epoch Seconds
  ** Again, use the Unix module.
  *)
  
  (* For the local timezone *)
  let ttup = mktime (localtime (time ())) ;;
  Printf.printf "Epoch Seconds (local): %.0f\n" (fst ttup) ;;
  
  (* For UTC *)
  let ttup = mktime (gmtime (time ())) ;;
  Printf.printf "Epoch Seconds (UTC): %.0f\n" (fst ttup) ;;
  

