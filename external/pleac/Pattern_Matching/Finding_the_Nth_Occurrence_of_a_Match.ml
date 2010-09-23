(* ********************************************************************** *)
(* Finding the Nth Occurrence of a Match *)
(* ********************************************************************** *)
let pleac_Finding_the_Nth_Occurrence_of_a_Match () = 
  #load "str.cma";;
  
  let want = 3
  let count = ref 0
  let pond = "One fish two fish red fish blue fish"
  let regexp = Str.regexp_case_fold "\\([a-z]+\\)[ ]+fish\\b"
  
  exception Found of string
  let () =
    let start = ref 0 in
    try
      while true do
        ignore (Str.search_forward regexp pond !start);
        start := !start + String.length (Str.matched_string pond);
        incr count;
        if !count = want then raise (Found (Str.matched_group 1 pond))
      done
    with
      | Found color ->
          Printf.printf "The third fish is a %s one.\n" color
      | Not_found ->
          Printf.printf "Only found %d fish!\n" !count
  
  (* The third fish is a red one. *)
  
  (*-----------------------------*)
  
  let colors =
    let start = ref 0 in
    let fish = ref [] in
    begin
      try
        while true do
          ignore (Str.search_forward regexp pond !start);
          start := !start + (String.length (Str.matched_string pond));
          fish := (Str.matched_group 1 pond) :: !fish
        done;
      with Not_found -> ()
    end;
    Array.of_list (List.rev !fish)
  
  let () =
    Printf.printf "The third fish in the pond is %s.\n" colors.(2)
  
  (* The third fish in the pond is red. *)
  
  (*-----------------------------*)
  
  let evens =
    let colors' = ref [] in
    Array.iteri
      (fun i color -> if i mod 2 = 1 then colors' := color :: !colors')
      colors;
    List.rev !colors'
  let () =
    Printf.printf "Even numbered fish are %s.\n" (String.concat " " evens)
  
  (* Even numbered fish are two blue. *)
  
  (*-----------------------------*)
  
  let () =
    let count = ref 0 in
    print_endline
      (Str.global_substitute
         (Str.regexp_case_fold "\\b\\([a-z]+\\)\\([ ]+fish\\b\\)")
         (fun s ->
            incr count;
            if !count = 4
            then "sushi" ^ Str.matched_group 2 s
            else Str.matched_group 1 s ^ Str.matched_group 2 s)
         pond)
  
  (* One fish two fish red fish sushi fish *)
  
  (*-----------------------------*)
  
  let pond = "One fish two fish red fish blue fish swim here."
  let regexp = Str.regexp_case_fold "\\b\\([a-z]+\\)[ ]+fish\\b"
  let colors =
    let rec loop start acc =
      try
        ignore (Str.search_forward regexp pond start);
        loop
          (start + String.length (Str.matched_string pond))
          (Str.matched_group 1 pond :: acc)
      with Not_found ->
        acc in
    loop 0 []
  let color = List.hd colors
  let () = Printf.printf "Last fish is %s.\n" color
  
  (* Last fish is blue. *)
  

