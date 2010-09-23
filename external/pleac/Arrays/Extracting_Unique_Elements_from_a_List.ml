(* ********************************************************************** *)
(* Extracting Unique Elements from a List *)
(* ********************************************************************** *)
let pleac_Extracting_Unique_Elements_from_a_List () = 
  
  (* For lists, the most "natural" way to do this is by walking the list and
   * looking for duplicates of each item *)
  
  let rec uniquesOnly l = 
    let rec contains x l =
      match l with
        [] -> false
      | h::t -> if x = h then true else contains x t in
    match l with 
      [] -> []
    | h::t -> if contains h t then uniquesOnly t else h::(uniquesOnly t);;
  
  (* if you have a lot of duplicates, it might be better to use List.filter *)
  let rec uniquesOnly l =
    match l with
      [] -> []
    | h::t -> h::(uniquesOnly (List.filter ((<>) h) t));;
  
  (* Or, for lists or arrays, you can use a hashtable *)
  (* Straightforward *)
  let uniquesOnly l =
    let seen = Hashtbl.create 17
    and uniq = ref [] in
    List.iter 
      (fun x -> 
        if not (Hashtbl.mem seen x) then 
          (Hashtbl.add seen x 1; uniq := (x::!uniq)))
      l;
    !uniq;;
  
  (* Or more likely *)
  let uniquesOnly l =
    let seen = Hashtbl.create 17 in
    List.iter (fun x -> Hashtbl.replace seen x 1) l;
    Hashtbl.fold (fun k v b -> k::b) seen [];;
  
  (* To apply a user function to each unique element of a list, one would likely
   * do something like *)
  
  let userUnique f l =
    List.map f (uniquesOnly l);;
  
  (* Generate a list of users logged in, removing duplicates.  Note that this
   * example requires linking with the Unix and Str libraries. *)
  let who () =
    let w = Unix.open_process_in "who"
    and l = ref [] in
    try
      while true do
        l := (input_line w)::!l
  	done;
  	!l
    with End_of_file -> !l;;
  
  let ucnt = Hashtbl.create 17;;
  List.iter 
    (fun x -> 
      Hashtbl.replace ucnt (Str.replace_first (Str.regexp "[ \t].*$") "" x) 1)
    (who ());;
  let users = Hashtbl.fold (fun k v b -> k::b) ucnt [];;
  
  printf "users logged in: %s";;
  List.iter (printf "%s ") users;;
  

