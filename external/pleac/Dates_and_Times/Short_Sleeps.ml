(* ********************************************************************** *)
(* Short Sleeps *)
(* ********************************************************************** *)
let pleac_Short_Sleeps () = 
  let usleep time =
    ignore (Unix.select [] [] [] time)
  
  let () =
    while true do
      usleep 0.25;
      print_newline ();
    done
  

