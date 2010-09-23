(* ********************************************************************** *)
(* Taking References to Arrays *)
(* ********************************************************************** *)
let pleac_Taking_References_to_Arrays () = 
  (* The following two sections use lists instead of arrays since
     list refs can be enlarged and copied easily. Also, arrays are
     mutable in OCaml, whereas lists are immutable. *)
  
  (* Create a reference to a list *)
  let lref      = ref list
  let anon_list = ref [9; 7; 5; 3; 1]
  let anon_copy = ref !anon_list
  
  let () =
    (* Add an item to the list *)
    anon_list := 11 :: !anon_list;
  
    (* Get the number of items from the list ref *)
    let num_items = List.length !anon_list in
  
    (* Print original data *)
    print_endline (String.concat ", "
                     (List.map (fun i -> string_of_int i) !anon_list));
  
    (* Sort it *)
    anon_list := List.sort compare !anon_list;
  
    (* Print sorted data *)
    print_endline (String.concat ", "
                     (List.map (fun i -> string_of_int i) !anon_list));
  

