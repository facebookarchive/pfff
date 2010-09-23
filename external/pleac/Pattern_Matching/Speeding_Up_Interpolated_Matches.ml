(* ********************************************************************** *)
(* Speeding Up Interpolated Matches *)
(* ********************************************************************** *)
let pleac_Speeding_Up_Interpolated_Matches () = 
  #load "str.cma";;
  
  let popstates = ["CO"; "ON"; "MI"; "WI"; "MN"]
  
  (* Naive version: Compile a regexp each time it is needed. *)
  let popgrep1 () =
    try
      begin
        while true do
          let line = input_line stdin in
          try
            List.iter
              (fun state ->
                 if (Str.string_match
                       (Str.regexp (".*\\b" ^ (Str.quote state) ^ "\\b"))
                       line 0)
                 then (print_endline line; raise Exit))
              popstates
          with Exit -> ()
        done
      end
    with End_of_file -> ()
  
  (* First optimization: Compile the regexps in advance. *)
  let popgrep2 () =
    let popstate_regexps =
      List.map
        (fun state ->
           Str.regexp (".*\\b" ^ (Str.quote state) ^ "\\b"))
        popstates in
    try
      begin
        while true do
          let line = input_line stdin in
          try
            List.iter
              (fun regexp ->
                 if (Str.string_match regexp line 0)
                 then (print_endline line; raise Exit))
              popstate_regexps
          with Exit -> ()
        done
      end
    with End_of_file -> ()
  
  (* Second optimization: Build a single regexp for all states. *)
  let popgrep3 () =
    let popstates_regexp =
      Str.regexp
        (".*\\b\\("
         ^ (String.concat "\\|" (List.map Str.quote popstates))
         ^ "\\)\\b") in
    try
      begin
        while true do
          let line = input_line stdin in
          if Str.string_match popstates_regexp line 0
          then print_endline line
        done
      end
    with End_of_file -> ()
  
  (* Speed tests with a 15,000 line input file: *)
  let () = popgrep1 ()         (* time: 13.670s *)
  let () = popgrep2 ()         (* time:  0.264s *)
  let () = popgrep3 ()         (* time:  0.123s *)
  

