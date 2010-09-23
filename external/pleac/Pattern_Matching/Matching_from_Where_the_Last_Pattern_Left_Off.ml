(* ********************************************************************** *)
(* Matching from Where the Last Pattern Left Off *)
(* ********************************************************************** *)
let pleac_Matching_from_Where_the_Last_Pattern_Left_Off () = 
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let s = "12 345 hello 6 7world89 10"
  let rex = Pcre.regexp "(\\d+)"
  
  let () =
    let subs = ref (Pcre.exec ~rex s) in
    try
      while true do
        Printf.printf "Found %s\n" (Pcre.get_substring !subs 1);
        subs := Pcre.next_match ~rex !subs
      done
    with Not_found -> ()
  
  (*-----------------------------*)
  
  let () =
    let n = "   49 here" in
    let n = Pcre.replace ~pat:"\\G " ~templ:"0" n in
    print_endline n
  
  (* 00049 here *)
  
  (*-----------------------------*)
  
  let s = "3,4,5,9,120"
  let rex = Pcre.regexp "\\G,?(\\d+)"
  
  let () =
    let subs = ref (Pcre.exec ~rex s) in
    try
      while true do
        Printf.printf "Found number %s\n" (Pcre.get_substring !subs 1);
        subs := Pcre.next_match ~rex !subs
      done
    with Not_found -> ()
  
  (*-----------------------------*)
  
  let s = "The year 1752 lost 10 days on the 3rd of September"
  
  let rex = Pcre.regexp "(\\d+)"
  let subs = ref (Pcre.exec ~rex s)
  
  let () =
    try
      while true do
        Printf.printf "Found number %s\n" (Pcre.get_substring !subs 1);
        subs := Pcre.next_match ~rex !subs
      done
    with Not_found -> ()
  
  let () =
    let rex = Pcre.regexp "\\G(\\S+)" in
    subs := Pcre.next_match ~rex !subs;
    Printf.printf "Found %s after the last number.\n"
      (Pcre.get_substring !subs 1)
  
  (*
    Found number 1752
    Found number 10
    Found number 3
    Found rd after the last number.
  *)
  
  (*-----------------------------*)
  
  let () =
    match Pcre.get_substring_ofs !subs 1 with
      | (start, finish) ->
          Printf.printf
            "The position in 's' is %d..%d\n" start finish
  
  (* The position in 's' is 35..37 *)
  

