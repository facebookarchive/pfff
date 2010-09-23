(* ********************************************************************** *)
(* Sorting a List by Computable Field *)
(* ********************************************************************** *)
let pleac_Sorting_a_List_by_Computable_Field () = 
  (* since compare orders tuples by first comparing the first slot then, if they
   * were equal, comparing the second slot, and so on, we can sort by computable
   * fields as follows *)
  
  let sorted = 
    List.map snd (List.sort compare (List.map (fun x-> (compute x),x) unsorted));;
  
  let ordered = List.sort (fun x y -> compare x#name y#name) employees;;
  List.iter (fun x -> printf "%s earns $%2f\n" x#name x#salary)
    (List.sort (fun x y -> compare x#name y#name) employees);;
  
  let sorted_employees = 
    List.map snd (List.sort compare (List.map (fun x-> (compute x),x) unsorted)) in
    List.iter (fun x -> printf "%s earns $%2f\n" x#name x#salary) sorted_employees;
    List.iter 
      (fun x -> if Hashtbl.mem bonus x#ssn then printf "%s got a bonus!\n" x#name)
      sorted_employees;;
  
  let sorted = 
    List.sort 
      (fun x y ->
        match compare x#name y#name with
          0 -> compare x#age y#age
        | c -> c)
      employees;;
  
  (* Assuming we have a getpwent function that returns a value of type users, or
   * throws an End_of_file exception when done (not sure what getpwent is supposed
   * to do), then we can write *)
  
  let getUsers () = 
    let l = ref [] in
    try
      while true do
        l := (getpwent ())::!l
      done
    with End_of_file -> !l;;
  
  List.iter 
    (fun x -> print_endline x#name) 
    (List.sort (fun x y -> compare x#name y#name) (getUsers ()));;
  
  let sorted = List.sort (fun x y -> compare x.[1] y.[1]) strings;;
  
  let sorted = 
    List.map snd
      (List.sort compare (List.map (fun x -> (String.length x),x) strings));;
  
  let sorted_fields = 
    List.map snd
      (List.sort compare 
        (List.map 
          (fun x ->
            (try 
              ignore(Str.search_forward (Str.regexp "[0-9]+") x 0);
              int_of_string (Str.matched_string x)
            with Not_found -> max_int),x) 
          strings));;
  
  let passwd () =
    let w = Unix.open_process_in "cat /etc/passwd"
    and l = ref [] in
    try
      while true do
        l := (input_line w)::!l
  	done;
  	!l
    with End_of_file -> !l;;
  
  (* for illustration purposes, we provide a function to return the (non-comment)
   * contents of /etc/passwd *)
  let passwd () =
    let w = Unix.open_process_in "cat /etc/passwd"
    and l = ref [] in
    try
      while true do
        l := (input_line w)::!l
  	done;
  	!l
    with End_of_file -> 
      List.filter (fun x -> x.[0] <> '#') !l;;
  
  let sortedPasswd = 
    List.map (fun Some x -> snd x)
    (List.sort compare
     (List.filter (function Some x -> true | None -> false)
      (List.map
        (fun x -> 
          match Str.split (Str.regexp ":") x with
            name::_::uid::gid::t -> Some ((gid,uid,name),x)
          | _ -> None) 
       (passwd ()))));;
  

