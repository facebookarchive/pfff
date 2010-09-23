(* ********************************************************************** *)
(* Printing a List with Commas *)
(* ********************************************************************** *)
let pleac_Printing_a_List_with_Commas () = 
  
  let commify_series l = 
    let rec sepChar l =
    match l with
      [] -> ", "
    | h::t -> 
         if String.contains h ',' then "; " else sepChar t in
    match l with
      [] -> ""
    | h::[] -> h
    | h1::h2::[] -> h1 ^ " and " ^ h2
    | _ ->
       let l' =
          let last::rest = List.rev l in
          (List.rev (("and " ^ last)::rest)) in
       String.concat (sepChar l) l';; 
  
  let lists = 
    [
      [ "just one thing" ];
      [ "Mutt"; "Jeff" ];
      [ "Peter"; "Paul"; "Mary" ];
      [ "To our parents"; "Mother Theresa"; "God" ];
      [ "pastrami"; "ham and cheese"; "peanut butter and jelly"; "tuna" ];
      [ "recycle tired, old phrases"; "ponder big, happy thoughts" ];
      [ "recycle tired, old phrases"; 
        "ponder big, happy thoughts"; 
        "sleep and dream peacefully" ]
    ];;
  
  List.iter (fun x -> printf "The list is: %s.\n" (commify_series x)) lists;;
  
  (* 
  The list is: just one thing.
  The list is: Mutt and Jeff.
  The list is: Peter, Paul, and Mary.
  The list is: To our parents, Mother Theresa, and God.
  The list is: pastrami, ham and cheese, peanut butter and jelly, and tuna.
  The list is: recycle tired, old phrases and ponder big, happy thoughts.
  The list is: recycle tired, old phrases; ponder big, happy thoughts; and sleep and dream peacefully.
  *)
  
  (* Note that if you are actually using arrays instead of lists, you can either
   * reuse the above code by calling "commify_series (Array.to_list a)", or you
   * can use the following solution (which won't work with lists, but is probably
   * more efficient).
  *)
  
  let commify_array a =
    let len = Array.length a in
    let rec sepChar a =
      try
        for i=0 to len - 1 do
          if String.contains a.(i) ',' then raise Not_found
        done;
        ", "
      with Not_found -> "; " in
    match len with
      0 -> ""
    | 1 -> a.(0)
    | 2 -> a.(0) ^ " and " ^ a.(1)
    | _ -> 
        let buf = Buffer.create 10
        and sep = sepChar a in
        for i = 0 to len - 2 do
          Buffer.add_string buf a.(i);
          Buffer.add_string buf sep;
        done;
        Buffer.add_string buf "and ";
        Buffer.add_string buf a.(len - 1);
        Buffer.contents buf;;
  
  let arrays = 
    [|
      [| "just one thing" |];
      [| "Mutt"; "Jeff" |];
      [| "Peter"; "Paul"; "Mary" |];
      [| "To our parents"; "Mother Theresa"; "God" |];
      [| "pastrami"; "ham and cheese"; "peanut butter and jelly"; "tuna" |];
      [| "recycle tired, old phrases"; "ponder big, happy thoughts" |];
      [| "recycle tired, old phrases"; 
        "ponder big, happy thoughts"; 
        "sleep and dream peacefully" |]
    |];;
  
  Array.iter (fun x -> printf "The list is: %s.\n" (commify_array x)) arrays;;
  

