(* ********************************************************************** *)
(* Matching Shell Globs as Regular Expressions *)
(* ********************************************************************** *)
let pleac_Matching_Shell_Globs_as_Regular_Expressions () = 
  #load "str.cma";;
  
  let regexp_string_of_glob s =
    let i, buffer = ref (-1), Buffer.create (String.length s + 8) in
    let read () =
      incr i;
      if !i < String.length s
      then Some s.[!i]
      else None in
    let write = Buffer.add_string buffer in
    let rec parse_glob () =
      match read () with
        | Some '*' -> write ".*"; parse_glob ()
        | Some '?' -> write "."; parse_glob ()
        | Some '[' -> parse_bracket ""
        | Some c -> write (Str.quote (String.make 1 c)); parse_glob ()
        | None -> ()
    and parse_bracket text =
      match read () with
        | Some '!' when text = "" -> parse_bracket "^"
        | Some ']' -> write ("[" ^ text ^ "]"); parse_glob ()
        | Some c -> parse_bracket (text ^ (String.make 1 c))
        | None -> write (Str.quote ("[" ^ text)) in
    write "^";
    parse_glob ();
    write "$";
    Buffer.contents buffer
  
  let regexp_of_glob s =
    Str.regexp (regexp_string_of_glob s)
  
  let regexp_of_glob_case_fold s =
    Str.regexp_case_fold (regexp_string_of_glob s)
  

