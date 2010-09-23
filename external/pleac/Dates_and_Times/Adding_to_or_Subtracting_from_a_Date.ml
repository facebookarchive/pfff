(* ********************************************************************** *)
(* Adding to or Subtracting from a Date *)
(* ********************************************************************** *)
let pleac_Adding_to_or_Subtracting_from_a_Date () = 
  let birthtime = 96176750.                     (* 18/Jan/1973, 3:45:50 am *)
  let interval = 5. +.                          (* 5 seconds *)
                 17. *. 60. +.                  (* 17 minutes *)
                 2.  *. 60. *. 60. +.           (* 2 hours *)
                 55. *. 60. *. 60. *. 24.       (* and 55 days *)
  let then' = birthtime +. interval
  let () =
    (* format_time is defined in section 3.8. *)
    Printf.printf "Then is %s\n" (format_time then');
    (* Then is Tue Mar 13 23:02:55 1973 *)
  

