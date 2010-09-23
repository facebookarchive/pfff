(* ********************************************************************** *)
(* Traversing a Hash *)
(* ********************************************************************** *)
let pleac_Traversing_a_Hash () = 
  (*-----------------------------*)
  (* in this section consider opened the Printf module using: *)
  open Printf;;
  
  Hashtbl.iter
    (fun key value ->
      (*
        do something with key and value
      *)
    )
    hash
  ;;
  (*-----------------------------*)
  List.iter (fun key ->
    let value = Hashtbl.find hash key in
      (*
        do something with key and value
      *)
  ) (hashtbl_keys hash)
  ;;
  (*-----------------------------*)
  (* food_color as defined in the introduction *)
  Hashtbl.iter (printf "%s is %s.\n") food_color;
  (*
  > Lemon is yellow.
  > Apple is red.
  > Carrot is orange.
  > Banana is yellow.
  *)
  (* but beware of: *)
  Hashtbl.iter (printf "food_color: %s is %s.\n") food_color;
  (*
  > food_color: Lemon is yellow.
  > Apple is red.
  > Carrot is orange.
  > Banana is yellow.
  *)
  (* write this instead:
    (more on it at http://caml.inria.fr/ocaml/htmlman/manual055.html) *)
  Hashtbl.iter (fun k v -> printf "food_color: %s is %s.\n" k v) food_color;
  (*
  > food_color: Lemon is yellow.
  > food_color: Apple is red.
  > food_color: Carrot is orange.
  > food_color: Banana is yellow.
  *)
  
  List.iter (fun key ->
    let value = Hashtbl.find food_color key in
    printf "%s is %s.\n" key value
  ) (hashtbl_keys food_color) ;
  (*
  > Lemon is yellow.
  > Apple is red.
  > Carrot is orange.
  > Banana is yellow.
  *)
  
  (*-----------------------------*)
  List.iter
    (fun key ->
      printf "%s is %s.\n" key (Hashtbl.find food_color key)
    )
    (sort_ (hashtbl_keys food_color))
  ;;
  
  (*
  > Apple is red.
  > Banana is yellow.
  > Carrot is orange.
  > Lemon is yellow.
  *)
  
  (*-----------------------------*)
  (* Ocaml is safe in loop, so you can't reset the hash iterator as in
  Perl and you don't risk infinite loops using, say, List.iter or
  Hashtbl.iter, but if you really want to infinite loop on the first key
  you get ... *)
  List.iter
    (fun key ->
      while true do
        printf "Processing %s\n" key
      done
    )
    (hashtbl_keys food_color)
  ;;
  (*-----------------------------*)
  (* countfrom - count number of messages from each sender *)
  let main () =
    let file =
      let files = ref [] in
      Arg.parse [] (fun file -> files := !files @ [file]) "";
      try
        open_in (List.hd !files)
      with Failure "hd" -> stdin
    in
    let from = Hashtbl.create 50 in
    let add_from address =
      let old_count =
        try Hashtbl.find from address
        with Not_found -> 0
      in
      let new_count = old_count + 1 in
      Hashtbl.replace from address new_count;
    in
    let extractfrom = Str.regexp "^From: \(.*\)" in
  
    iter_lines (fun line ->
      if (Str.string_match extractfrom line 0) then
        add_from (Str.matched_group 1 line)
      else ()
    ) file;
    Hashtbl.iter (printf "%s: %d\n") from
  ;;
  main() ;
  

