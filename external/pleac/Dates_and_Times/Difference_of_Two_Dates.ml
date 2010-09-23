(* ********************************************************************** *)
(* Difference of Two Dates *)
(* ********************************************************************** *)
let pleac_Difference_of_Two_Dates () = 
  let bree = 361535725.                   (* 16 Jun 1981, 4:35:25 *)
  let nat  =  96201950.                   (* 18 Jan 1973, 3:45:50 *)
  
  let difference = bree -. nat
  let () =
    Printf.printf "There were %.f seconds between Nat and Bree\n"
      difference
    (* There were 265333775 seconds between Nat and Bree *)
  
  let seconds    =  mod_float difference 60.
  let difference = (difference -. seconds) /. 60.
  let minutes    =  mod_float difference 60.
  let difference = (difference -. minutes) /. 60.
  let hours      =  mod_float difference 24.
  let difference = (difference -. hours)   /. 24.
  let days       =  mod_float difference 7.
  let weeks      = (difference -. days)    /.  7.
  
  let () =
    Printf.printf "(%.f weeks, %.f days, %.f:%.f:%.f)\n"
      weeks days hours minutes seconds
    (* (438 weeks, 4 days, 23:49:35) *)
  

