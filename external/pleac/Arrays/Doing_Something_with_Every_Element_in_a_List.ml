(* ********************************************************************** *)
(* Doing Something with Every Element in a List *)
(* ********************************************************************** *)
let pleac_Doing_Something_with_Every_Element_in_a_List () = 
  
  Array.iter complain bad_users;;
  (* Or for lists *)
  List.iter complain bad_users;;
  
  (* For the hashtable example, we'd iterate over the table itself *)
  
  Hashtbl.iter (fun k v -> printf "%s=%s\n" k v) h;; 
  
  (* Of course if you want to iterate over the keys in lexicographic order, then
   * you'll need to build a list of keys, sort it, then iterate over that *)
  
  List.iter (fun x -> printf "%s=%s\n" x (Hashtbl.find env x))
    (List.sort compare (Hashtbl.fold (fun k v b -> k::b) env []));;
  
  Array.iter (fun x -> if get_usage x > max_quota then complain x) all_users;;
  (* or for lists of users *)
  List.iter (fun x -> if get_usage x > max_quota then complain x) all_users;;
  
  (* for this example, we're going to assume that the output of the who command is
   * contained in the list named who, with one line of output per list element.
   * This example requires the use of the Str module which is not loaded or linked
   * by default (but is part of the standard library), at the toplevel, use the
   * directive "#load "str.cma"
  *)
  
  List.iter 
    (fun x -> 
      try 
        ignore (Str.search_forward (Str.quote "tchrist") x 0);
        print_endline x;
      with Not_found -> ()) who;;
  
  (* To iterate over all lines read in from some channel we would do the following *)
    
  let iter_channel f ic =
    try
      while true do
        f (input_line ic)
      done
    with Not_found -> ();;
  
  (* and the example would then be written as *)
  iter_channel
    (fun  s ->
      let reverse s ='let len = String.length s in
        let s' = String.create len in
        for i = 0 to len - 1 do
          s'.[len-i-1] <- s.[i]
        done;
        s' in
      (* assuming we have written a chomp workalike *)
      let s = chomp s in
      List.iter 
        (fun x -> print_endline (reverse x)) 
        (Str.split (Str.regexp "[ \t]+") s)) fh;;
  
  (* In OCaml the iterator variable also is an alias for the current element,
   * however, because of the functional nature of OCaml, unless the elements of
   * the array are references, the only way to change them is by resetting the
   * value of the array to something new -- this is best done using iteri *)
  
  let a = [|1; 2; 3|];;
  Array.iteri (fun i x -> a.(i) <- x-1) a;;
  
  (* or, with references *)
  
  let a = [| ref 1; ref 2; ref 3 |];;
  Array.iter (fun x -> x := !x - 1) a;;
  
  (* You can, of course, use map to create a new array with the desired contents
   * as well *)
  let a = [| 0.5; 3.|];;
  let b = [|0.; 1.|];;
  Array.iter (printf "%f ") (Array.map (( *. ) 7.) (Array.append a b));;
  
  
  let strip s =
    Str.replace_first (Str.regexp "^[ \t\n]") ""
      (Str.replace_first (Str.regexp "[ \t\n$]") "" s);;
  
  
  let sc,ar,h = 
    strip sc,
    Array.map strip ar,
    (Hashtbl.iter (fun k v -> Hashtbl.replace h k (strip v)) h; h);;
  
  (* of course, the Hashtbl.replace already destructively updates the old
   * hashtable... *)
  

