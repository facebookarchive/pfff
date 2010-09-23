(* ********************************************************************** *)
(* Sorting an Array Numerically *)
(* ********************************************************************** *)
let pleac_Sorting_an_Array_Numerically () = 
  
  (* OCaml is smart enough to figure out if a list is full of numbers or
   * non-numbers, so the polymorphic compare function works just fine *)
  let sorted = List.sort compare unsorted;;
  
  (* note that Array.sort sorts the given array in place, so unexpected results
   * can occur, e.g.
  let sorted = Array.sort compare unsorted;;
  
   * results in unsorted referring to the now sorted array, and sorted referring
   * to something of type unit *)
  
  (* pids is an unsorted list of process IDs *)
  List.iter (printf "%d\n") (List.sort compare pids);;
  print_endline "Select a process ID to kill:";;
  let pid = read_int () in
    Unix.kill pid Sys.sigterm;
    Unix.sleep 2;
    Unix.kill pid Sys.sigterm;;
  
  let descending = List.sort (fun x y -> compare y x) unsorted;;
  

