(* ********************************************************************** *)
(* Expressing AND, OR, and NOT in a Single Pattern *)
(* ********************************************************************** *)
let pleac_Expressing_AND__OR__and_NOT_in_a_Single_Pattern () = 
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let pat = input_line config_channel
  let () = if Pcre.pmatch ~pat data then (* ... *) ()
  
  (*-----------------------------*)
  
  (* alpha OR beta *)
  let regexp = Pcre.regexp "alpha|beta"
  
  (* alpha AND beta *)
  let regexp = Pcre.regexp ~flags:[`DOTALL] "^(?=.*alpha)(?=.*beta)"
  
  (* alpha AND beta, no overlap *)
  let regexp = Pcre.regexp ~flags:[`DOTALL] "alpha.*beta|beta.*alpha"
  
  (* NOT pat *)
  let regexp = Pcre.regexp ~flags:[`DOTALL] "^(?:(?!pat).)*$"
  
  (* NOT bad BUT good *)
  let regexp = Pcre.regexp ~flags:[`DOTALL] "(?=(?:(?!bad).)*$)good"
  
  (*-----------------------------*)
  
  let () =
    if not (Pcre.pmatch ~rex:regexp text)
    then something ()
  
  (*-----------------------------*)
  
  let () =
    if (Pcre.pmatch ~rex:rexexp1 text) && (Pcre.pmatch ~rex:rexexp2 text)
    then something ()
  
  (*-----------------------------*)
  
  let () =
    if (Pcre.pmatch ~rex:rexexp1 text) || (Pcre.pmatch ~rex:rexexp2 text)
    then something ()
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* minigrep - trivial grep *)
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  let minigrep pat files =
    let rex =
      try Pcre.regexp pat
      with Pcre.BadPattern (msg, _) ->
        Printf.eprintf "%s: Bad pattern %s: %s\n" Sys.argv.(0) pat msg;
        exit 1 in
    let process file =
      let channel = if file = "-" then stdin else open_in file in
      try
        Stream.iter
          (fun line -> if Pcre.pmatch ~rex line then print_endline line)
          (line_stream_of_channel channel);
        close_in channel
      with e ->
        close_in channel;
        raise e in
    List.iter process files
  
  let () =
    match List.tl (Array.to_list Sys.argv) with
      | pat :: [] -> minigrep pat ["-"]
      | pat :: files -> minigrep pat files
      | [] -> Printf.eprintf "usage: %s pat [files]\n" Sys.argv.(0)
  
  (*-----------------------------*)
  
  let string = "labelled"
  
  let () =
    Printf.printf "%b\n"
      (Pcre.pmatch
         ~rex:(Pcre.regexp ~flags:[`DOTALL] "^(?=.*bell)(?=.*lab)")
         string)
  
  let () =
    Printf.printf "%b\n"
      (Pcre.pmatch ~pat:"bell" string && Pcre.pmatch ~pat:"lab" string)
  
  let () =
    if (Pcre.pmatch
          ~rex:(Pcre.regexp ~flags:[`DOTALL; `EXTENDED] "
               ^              # start of string
              (?=             # zero-width lookahead
                  .*          # any amount of intervening stuff
                  bell        # the desired bell string
              )               # rewind, since we were only looking
              (?=             # and do the same thing
                  .*          # any amount of intervening stuff
                  lab         # and the lab part
              )")
          string)
    then print_endline "Looks like Bell Labs might be in Murray Hill!"
  
  let () =
    Printf.printf "%b\n"
      (Pcre.pmatch ~pat:"(?:^.*bell.*lab)|(?:^.*lab.*bell)" string)
  
  let brand = "labelled"
  let () =
    if (Pcre.pmatch
          ~rex:(Pcre.regexp ~flags:[`DOTALL; `EXTENDED] "
          (?:                 # non-capturing grouper
              ^ .*?           # any amount of stuff at the front
                bell          # look for a bell
                .*?           # followed by any amount of anything
                lab           # look for a lab
            )                 # end grouper
      |                       # otherwise, try the other direction
          (?:                 # non-capturing grouper
              ^ .*?           # any amount of stuff at the front
                lab           # look for a lab
                .*?           # followed by any amount of anything
                bell          # followed by a bell
            )                 # end grouper
        ") brand)
    then print_endline "Our brand has bell and lab separate."
  
  let map = "a map of the world"
  let () =
    Printf.printf "%b\n"
      (Pcre.pmatch
         ~rex:(Pcre.regexp ~flags:[`DOTALL] "^(?:(?!waldo).)*$")
         map)
  
  let () =
    if (Pcre.pmatch
          ~rex:(Pcre.regexp ~flags:[`DOTALL; `EXTENDED] "
          ^                   # start of string
          (?:                 # non-capturing grouper
              (?!             # look ahead negation
                  waldo       # is he ahead of us now?
              )               # is so, the negation failed
              .               # any character (cuzza /s)
          ) *                 # repeat that grouping 0 or more
          $                   # through the end of the string
        ") map)
    then print_endline "There's no waldo here!"
  
  (*-----------------------------*)
  
  % w | minigrep '^(?!.*ttyp).*tchrist'
  
  (*-----------------------------*)
  
  Pcre.regexp ~flags:[`EXTENDED] "
      ^                       # anchored to the start
      (?!                     # zero-width look-ahead assertion
          .*                  # any amount of anything (faster than .*?)
          ttyp                # the string you don't want to find
      )                       # end look-ahead negation; rewind to start
      .*                      # any amount of anything (faster than .*?)
      tchrist                 # now try to find Tom
  "
  
  (*-----------------------------*)
  
  % w | grep tchrist | grep -v ttyp
  
  (*-----------------------------*)
  
  % grep -i 'pattern' files
  % minigrep '(?i)pattern' files
  

