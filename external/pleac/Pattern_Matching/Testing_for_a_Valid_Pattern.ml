(* ********************************************************************** *)
(* Testing for a Valid Pattern *)
(* ********************************************************************** *)
let pleac_Testing_for_a_Valid_Pattern () = 
  #load "str.cma";;
  let () =
    while true do
      print_string "Pattern? ";
      flush stdout;
      let pattern = input_line stdin in
      try ignore (Str.regexp pattern)
      with Failure message ->
        Printf.printf "INVALID PATTERN: %s\n" message
    done
  
  (*-----------------------------*)
  
  let is_valid_pattern pattern =
    try ignore (Str.regexp pattern); true
    with Failure _ -> false
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* paragrep - trivial paragraph grepper *)
  #load "str.cma";;
  
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
  
  let paragrep pat files =
    let regexp =
      begin
        try Str.regexp pat
        with Failure msg ->
          Printf.eprintf "%s: Bad pattern %s: %s\n" Sys.argv.(0) pat msg;
          exit 1
      end in
    let count = ref 0 in
    List.iter
      (fun file ->
         let channel =
           if file = "-"
           then stdin
           else open_in file in
         try
           Stream.iter
             (fun para ->
                incr count;
                try
                  ignore (Str.search_forward regexp para 0);
                  Printf.printf "%s %d: %s\n\n" file !count para
                with Not_found -> ())
             (paragraph_stream_of_channel channel);
           close_in channel
         with e ->
           close_in channel;
           raise e)
      files
  
  let () =
    match List.tl (Array.to_list Sys.argv) with
      | pat :: [] -> paragrep pat ["-"]
      | pat :: files -> paragrep pat files
      | [] -> Printf.eprintf "usage: %s pat [files]\n" Sys.argv.(0)
  
  (*-----------------------------*)
  
  let safe_pat = Str.quote pat
  

