(* ********************************************************************** *)
(* Matching a Valid Mail Address *)
(* ********************************************************************** *)
let pleac_Matching_a_Valid_Mail_Address () = 
  #load "str.cma";;
  
  (* Not foolproof, but works in most common cases. *)
  let regexp =
    Str.regexp_case_fold
      "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z][A-Z][A-Z]?[A-Z]?\\b"
  
  let () =
    try
      while true do
        print_string "Email: ";
        flush stdout;
        let line = input_line stdin in
        try
          let start = ref 0 in
          while true do
            start := Str.search_forward regexp line !start;
            let string = Str.matched_string line in
            start := !start + String.length string;
            print_string "Found: ";
            print_endline string;
          done
        with Not_found -> ()
      done
    with End_of_file -> ()
  

