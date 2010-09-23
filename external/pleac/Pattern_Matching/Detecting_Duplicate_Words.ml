(* ********************************************************************** *)
(* Detecting Duplicate Words *)
(* ********************************************************************** *)
let pleac_Detecting_Duplicate_Words () = 
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  let paragraph_stream_of_channel channel =
    let lines = line_stream_of_channel channel in
    let rec next para_lines i =
      match Stream.peek lines, para_lines with
        | None, [] -> None
        | Some "", [] -> Stream.junk lines; next para_lines i
        | Some "", _
        | None, _ -> Some (String.concat "\n" (List.rev para_lines))
        | Some line, _ -> Stream.junk lines; next (line :: para_lines) i in
    Stream.from (next [])
  
  let find_dup_words files =
    let rex = Pcre.regexp ~flags:[`CASELESS; `EXTENDED] "
        \\b            # start at a word boundary (begin letters)
        (\\S+)         # find chunk of non-whitespace
        \\b            # until another word boundary (end letters)
        (
            \\s+       # separated by some whitespace
            \\1        # and that very same chunk again
            \\b        # until another word boundary
        ) +            # one or more sets of those
    " in
    let count = ref 0 in
    List.iter
      (fun file ->
         let channel = if file = "-" then stdin else open_in file in
         try
           Stream.iter
             (fun para ->
                incr count;
                try
                  let subs = ref (Pcre.exec ~rex para) in
                  while true do
                    Printf.printf "dup word '%s' at paragraph %d.\n"
                      (Pcre.get_substring !subs 1)
                      !count;
                    flush stdout;
                    subs := Pcre.next_match ~rex !subs;
                  done
                with Not_found -> ())
             (paragraph_stream_of_channel channel);
           close_in channel
         with e ->
           close_in channel;
           raise e)
      files
  
  let () =
    match List.tl (Array.to_list Sys.argv) with
      | [] -> find_dup_words ["-"]
      | files -> find_dup_words files
  
  (*-----------------------------*)
  
  (*
    This is a test
    test of the duplicate word finder.
  *)
  
  (* dup word 'test' at paragraph 1. *)
  
  (*-----------------------------*)
  
  let a = "nobody"
  let b = "bodysnatcher"
  let () =
    try
      let subs =
        Pcre.exec
          ~pat:"^(\\w+)(\\w+) \\2(\\w+)$"
          (a ^ " " ^ b) in
      Printf.printf "%s overlaps in %s-%s-%s\n"
        (Pcre.get_substring subs 2)
        (Pcre.get_substring subs 1)
        (Pcre.get_substring subs 2)
        (Pcre.get_substring subs 3)
    with Not_found ->
      ()
  
  (* body overlaps in no-body-snatcher *)
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* prime_pattern -- find prime factors of argument using pattern matching *)
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let arg = try int_of_string Sys.argv.(1) with _ -> 0
  let n = ref (String.make arg 'o')
  let rex = Pcre.regexp "^(oo+?)\\1+$"
  let templ = "o"
  let () =
    try
      while true do
        let pat = Pcre.get_substring (Pcre.exec ~rex !n) 1 in
        Printf.printf "%d " (String.length pat);
        n := Pcre.replace ~pat ~templ !n
      done
    with Not_found ->
      Printf.printf "%d\n" (String.length !n)
  
  (*-----------------------------*)
  
  exception Found of (int * int * int)
  let () =
    try
      match
        Pcre.extract
          ~full_match:false
          ~pat:"^(o*)\\1{11}(o*)\\2{14}(o*)\\3{15}$"
          (String.make 281 'o')
      with
        | [| x; y; z |] -> raise (Found
                                    (String.length x,
                                     String.length y,
                                     String.length z))
        | _ -> raise Not_found
    with
      | Found (x, y, z) ->
          Printf.printf "One solution is: x=%d; y=%d; z=%d.\n"
            x y z
      | Not_found ->
          Printf.printf "No solution.\n"
  
  (* One solution is: x=17; y=3; z=2. *)
  
  (*-----------------------------*)
  
  ~pat:"^(o+)\\1{11}(o+)\\2{14}(o+)\\3{15}$"
  (* One solution is: x=17; y=3; z=2. *)
  
  ~pat:"^(o*?)\\1{11}(o*)\\2{14}(o*)\\3{15}$"
  (* One solution is: x=0; y=7; z=11. *)
  
  ~pat:"^(o+?)\\1{11}(o*)\\2{14}(o*)\\3{15}$"
  (* One solution is: x=1; y=3; z=14. *)
  

