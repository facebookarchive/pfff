(* ********************************************************************** *)
(* Writing a Filter *)
(* ********************************************************************** *)
let pleac_Writing_a_Filter () = 
  #load "str.cma";;
  
  let parse_args () =
    match List.tl (Array.to_list Sys.argv) with
      | [] -> ["-"]
      | args -> args
  
  let run_filter func args =
    List.iter
      (fun arg ->
         let in_channel =
           match arg with
             | "-" -> stdin
             | arg -> open_in arg in
         try
           begin
             try
               while true do
                 func (input_line in_channel)
               done
             with End_of_file -> ()
           end;
           close_in in_channel
         with e ->
           close_in in_channel;
           raise e)
      args
  
  let () =
    run_filter
      (fun line ->
         (* do something with the line *)
         ())
      (parse_args ())
  
  (*-----------------------------*)
  
  (* arg demo 1: Process optional -c flag *)
  let chop_first = ref false
  let args =
    match parse_args () with
      | "-c" :: rest -> chop_first := true; rest
      | args -> args
  
  (* arg demo 2: Process optional -NUMBER flag *)
  let columns = ref None
  let args =
    match parse_args () with
      | arg :: rest
        when Str.string_match (Str.regexp "^-\\([0-9]+\\)$") arg 0 ->
          columns := Some (int_of_string (Str.matched_group 1 arg));
          rest
      | args -> args
  
  (* arg demo 3: Process clustering -a, -i, -n, or -u flags *)
  let append = ref false
  let ignore_ints = ref false
  let nostdout = ref false
  let unbuffer = ref false
  let args =
    let rec parse_flags = function
      | "" -> ()
      | s ->
          (match s.[0] with
             | 'a' -> append      := true
             | 'i' -> ignore_ints := true
             | 'n' -> nostdout    := true
             | 'u' -> unbuffer    := true
             | _ ->
                 Printf.eprintf "usage: %s [-ainu] [filenames] ...\n"
                   Sys.argv.(0);
                 flush stderr;
                 exit 255);
          parse_flags (String.sub s 1 (String.length s - 1)) in
    List.rev
      (List.fold_left
         (fun acc ->
            function
              | "" -> acc
              | s when s.[0] = '-' ->
                  parse_flags (String.sub s 1 (String.length s - 1));
                  acc
              | arg -> arg :: acc)
         []
         (parse_args ()))
  
  (*-----------------------------*)
  
  (* findlogin - print all lines containing the string "login" *)
  
  let () =
    run_filter
      (fun line ->
         if Str.string_match (Str.regexp ".*login.*") line 0
         then print_endline line)
      (parse_args ())
  
  (*-----------------------------*)
  
  (* lowercase - turn all lines into lowercase *)
  
  let () =
    run_filter
      (fun line -> print_endline (String.lowercase line))
      (parse_args ())
  
  (*-----------------------------*)
  
  (* countchunks - count how many words are used *)
  
  let chunks = ref 0
  let () =
    run_filter
      (fun line ->
         if line <> "" && line.[0] == '#'
         then ()
         else chunks := !chunks
           + List.length (Str.split (Str.regexp "[ \t]+") line))
      (parse_args ());
    Printf.printf "Found %d chunks\n" !chunks
  

