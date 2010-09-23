(* ********************************************************************** *)
(* Ringing the Terminal Bell *)
(* ********************************************************************** *)
let pleac_Ringing_the_Terminal_Bell () = 
  (* OCaml doesn't recognize '\a'; instead use '\007'. *)
  let () = print_endline "\007Wake up!"
  
  (* Use the "tput" command to produce a visual bell. *)
  let () = ignore (Sys.command "tput flash")
  

