(* ********************************************************************** *)
(* Reading Records with a Pattern Separator *)
(* ********************************************************************** *)
let pleac_Reading_Records_with_a_Pattern_Separator () = 
  #load "str.cma";;
  let chunks =
    let lines = ref [] in
    begin
      try while true do lines := input_line stdin :: !lines done
      with End_of_file -> ()
    end;
    let contents = String.concat "\n" (List.rev !lines) in
    Str.full_split (Str.regexp "^\\.\\(Ch\\|Se\\|Ss\\)$") contents
  let () =
    Printf.printf
      "I read %d chunks.\n"
      (List.length chunks)
  

