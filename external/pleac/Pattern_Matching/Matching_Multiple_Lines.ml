(* ********************************************************************** *)
(* Matching Multiple Lines *)
(* ********************************************************************** *)
let pleac_Matching_Multiple_Lines () = 
  #!/usr/bin/ocaml
  (* killtags - very bad html tag killer *)
  #load "str.cma";;
  let regexp = Str.regexp "<[^>]*>"
  let () =
    List.iter
      (fun filename ->
         let lines = ref [] in
         let in_channel = open_in filename in
         try
           begin
             try while true do lines := input_line in_channel :: !lines done
             with End_of_file -> ()
           end;
           let contents = String.concat "\n" (List.rev !lines) in
           print_endline
             (String.concat ""
                (List.map
                   (function
                      | Str.Text s -> s
                      | _ -> "")
                   (Str.full_split regexp contents)));
           close_in in_channel
         with e ->
           close_in in_channel;
           raise e)
      (List.tl (Array.to_list Sys.argv))
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* headerfy - change certain chapter headers to html *)
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
  
  let regexp = Str.regexp "^Chapter[\r\n\t ]+[0-9]+[\r\n\t ]*:[^\r\n]*"
  
  let headerfy chunk =
    String.concat ""
      (List.map
         (function
            | Str.Text s -> s
            | Str.Delim s -> "<H1>" ^ s ^ "</H1>")
         (Str.full_split regexp chunk))
  
  let () =
    List.iter
      (fun filename ->
         let in_channel = open_in filename in
         try
           Stream.iter
             (fun para ->
                print_endline (headerfy para);
                print_newline ())
             (paragraph_stream_of_channel in_channel);
           close_in in_channel
         with e ->
           close_in in_channel;
           raise e)
      (List.tl (Array.to_list Sys.argv))
  

