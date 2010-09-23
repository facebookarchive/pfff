(* ********************************************************************** *)
(* Matching Abbreviations *)
(* ********************************************************************** *)
let pleac_Matching_Abbreviations () = 
  #load "str.cma";;
  
  let () =
    try
      while true do
        print_string "Action: ";
        flush stdout;
        let answer = input_line stdin in
        let regexp = Str.regexp_string_case_fold answer in
        if Str.string_match regexp "SEND" 0
        then print_endline "Action is send"
        else if Str.string_match regexp "STOP" 0
        then print_endline "Action is stop"
        else if Str.string_match regexp "ABORT" 0
        then print_endline "Action is abort"
        else if Str.string_match regexp "LIST" 0
        then print_endline "Action is list"
        else if Str.string_match regexp "EDIT" 0
        then print_endline "Action is edit"
      done
    with End_of_file -> ()
  
  (*-----------------------------*)
  
  (* assumes that invoke_editor, deliver_message, *)
  (* file and pager are defined somewhere else. *)
  let actions =
    [
      "edit", invoke_editor;
      "send", deliver_message;
      "list", (fun () -> ignore (Sys.command (pager ^ " " ^ file)));
      "abort", (fun () -> print_endline "See ya!"; exit 0);
    ]
  
  let errors = ref 0
  
  let () =
    try
      while true do
        print_string "Action: ";
        flush stdout;
        let answer = input_line stdin in
        (* trim leading white space *)
        let answer = Str.replace_first (Str.regexp "^[ \t]+") "" answer in
        (* trim trailing white space *)
        let answer = Str.replace_first (Str.regexp "[ \t]+$") "" answer in
        let regexp = Str.regexp_string_case_fold answer in
        let found = ref false in
        List.iter
          (fun (action, handler) ->
             if Str.string_match regexp action 0
             then (found := true; handler ()))
          actions;
        if not !found
        then (incr errors; print_endline ("Unknown command: " ^ answer))
      done
    with End_of_file -> ()
  

