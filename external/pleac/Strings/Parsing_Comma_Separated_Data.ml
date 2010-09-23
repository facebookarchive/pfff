(* ********************************************************************** *)
(* Parsing Comma-Separated Data *)
(* ********************************************************************** *)
let pleac_Parsing_Comma_Separated_Data () = 
  
  let parse_csv =
    let regexp = Str.regexp (String.concat "\\|" [
                               "\"\\([^\"\\\\]*\\(\\\\.[^\"\\\\]*\\)*\\)\",?";
                               "\\([^,]+\\),?";
                               ",";
                             ]) in
    fun text ->
      let rec loop start result =
        if Str.string_match regexp text start then
          let result =
            (try Str.matched_group 1 text with Not_found ->
               try Str.matched_group 3 text with Not_found ->
                 "") :: result in
          loop (Str.match_end ()) result
        else
          result in
      List.rev ((if
                   try String.rindex text ',' = String.length text - 1
                   with Not_found -> false
                 then [""] else [])
                @ loop 0 [])
  
  let line = "XYZZY,\"\",\"O'Reilly, Inc\",\"Wall, Larry\",\"a \\\"glug\\\" bit,\",5,\"Error, Core Dumped\""
  let () =
    Array.iteri
      (fun i x -> Printf.printf "%d : %s\n" i x)
      (Array.of_list (parse_csv line))
  

