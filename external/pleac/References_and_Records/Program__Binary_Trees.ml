(* ********************************************************************** *)
(* Program: Binary Trees *)
(* ********************************************************************** *)
let pleac_Program__Binary_Trees () = 
  (* bintree - binary tree demo program *)
  
  type 'a tree = { value : 'a;
                   left  : 'a tree option;
                   right : 'a tree option }
  
  let rec string_of_tree tree =
    Printf.sprintf "{ value = %d; left = %s; right = %s }"
      tree.value
      (match tree.left with
         | None -> "None"
         | Some tree -> Printf.sprintf "Some (%s)" (string_of_tree tree))
      (match tree.right with
         | None -> "None"
         | Some tree -> Printf.sprintf "Some (%s)" (string_of_tree tree))
  
  (* insert given value into proper point of
     provided tree.  If no tree provided,
     fill one in for our caller. *)
  let rec insert tree value =
    match tree with
      | None -> { value = value; left = None; right = None }
      | Some tree ->
          if tree.value > value
          then { value = tree.value;
                 left  = Some (insert tree.left value);
                 right = tree.right }
          else if tree.value < value
          then { value = tree.value;
                 left  = tree.left;
                 right = Some (insert tree.right value) }
          else tree
  
  (* recurse on left child,
     then show current value,
     then recurse on right child. *)
  let rec in_order tree =
    match tree with
      | None -> ()
      | Some tree ->
          in_order tree.left;
          print_int tree.value;
          print_string " ";
          in_order tree.right
  
  (* show current value,
     then recurse on left child,
     then recurse on right child. *)
  let rec pre_order tree =
    match tree with
      | None -> ()
      | Some tree ->
          print_int tree.value;
          print_string " ";
          pre_order tree.left;
          pre_order tree.right
  
  (* recurse on left child,
     then recurse on right child,
     then show current value. *)
  let rec post_order tree =
    match tree with
      | None -> ()
      | Some tree ->
          post_order tree.left;
          post_order tree.right;
          print_int tree.value;
          print_string " "
  
  (* find out whether provided value is in the tree.
     if so, return the node at which the value was found.
     cut down search time by only looking in the correct
     branch, based on current value. *)
  let rec search tree value =
    match tree with
      | Some tree ->
          if tree.value = value
          then Some tree
          else search (if value < tree.value then tree.left else tree.right) value
      | None -> None
  
  (* reference to the root of the tree *)
  let root = ref None
  
  (* first generate 20 random inserts *)
  let () =
    Random.self_init ();
    for n = 0 to 19 do
      root := Some (insert !root (Random.int 1000))
    done
  
  (* now dump out the tree all three ways *)
  let () =
    print_string "Pre order: "; pre_order !root; print_newline ();
    print_string "In order: "; in_order !root; print_newline ();
    print_string "Post order: "; post_order !root; print_newline ()
  
  (* prompt until EOF *)
  let () =
    try
      while true do
        let line = read_line () in
        let num = int_of_string line in
        let found = search !root num in
        match found with
          | Some tree ->
              Printf.printf "Found %d at %s, %d\n"
                num
                (string_of_tree tree)
                tree.value
          | None ->
              Printf.printf "No %d in the tree\n" num
      done
    with End_of_file ->
      ()
  

