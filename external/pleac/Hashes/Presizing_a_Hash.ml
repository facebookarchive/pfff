(* ********************************************************************** *)
(* Presizing a Hash *)
(* ********************************************************************** *)
let pleac_Presizing_a_Hash () = 
  (*-----------------------------*)
  
  (* presize hash to num elements *)
  let hash = Hashtbl.create num;;
  (* other examples of initial size on hashes *)
  let hash = Hashtbl.create 512;;
  let hash = Hashtbl.create 1000;;
  
  (*-----------------------------*)

