(* ********************************************************************** *)
(* High-Resolution Timers *)
(* ********************************************************************** *)
let pleac_High_Resolution_Timers () = 
  #load "unix.cma";;
  
  let t0 = Unix.gettimeofday ()
  let () = print_string "Press return when ready: "; ignore (read_line ())
  let t1 = Unix.gettimeofday ()
  let () = Printf.printf "You took %f seconds.\n" (t1 -. t0)
  
  (*-----------------------------*)
  
  let size = 500 in
  let number_of_times = 100 in
  let total_time = ref 0. in
  
  for i = 1 to number_of_times do
    let array = Array.init size (fun _ -> Random.bits()) in
  
    let before = Unix.gettimeofday() in
    Array.stable_sort compare array ;
    let time = Unix.gettimeofday() -. before in
    total_time := !total_time +. time
  done ;
  
  Printf.printf "On average, sorting %d random numbers takes %.5f seconds\n" size (!total_time /. float number_of_times)
  

