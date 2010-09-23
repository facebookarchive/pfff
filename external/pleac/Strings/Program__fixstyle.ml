(* ********************************************************************** *)
(* Program: fixstyle *)
(* ********************************************************************** *)
let pleac_Program__fixstyle () = 
  (* fixstyle - switch first set of data strings to second set *)
  #load "str.cma";;
  
  let data = Hashtbl.create 0
  let keys = ref []
  let () =
    let ( => ) key value =
      keys := key :: !keys;
      Hashtbl.replace data key value in
    (
      "analysed"       => "analyzed";
      "built-in"       => "builtin";
      "chastized"      => "chastised";
      "commandline"    => "command-line";
      "de-allocate"    => "deallocate";
      "dropin"         => "drop-in";
      "hardcode"       => "hard-code";
      "meta-data"      => "metadata";
      "multicharacter" => "multi-character";
      "multiway"       => "multi-way";
      "non-empty"      => "nonempty";
      "non-profit"     => "nonprofit";
      "non-trappable"  => "nontrappable";
      "pre-define"     => "predefine";
      "preextend"      => "pre-extend";
      "re-compiling"   => "recompiling";
      "reenter"        => "re-enter";
      "turnkey"        => "turn-key";
    )
  
  let pattern_text =
    "\\(" ^ (String.concat "\\|" (List.map Str.quote !keys)) ^ "\\)"
  let pattern = Str.regexp pattern_text
  
  let args = ref (List.tl (Array.to_list Sys.argv))
  
  let verbose =
    match !args with
      | "-v" :: rest -> args := rest; true
      | _ -> false
  
  let () =
    if !args = []
    then (Printf.eprintf "%s: reading from stdin\n" Sys.argv.(0);
          args := ["-"])
  
  let replace_all text line file =
    String.concat ""
      (List.map
         (function
            | Str.Text s -> s
            | Str.Delim s ->
                if verbose
                then Printf.eprintf "%s => %s at %s line %d.\n"
                  s (Hashtbl.find data s) file line;
                Hashtbl.find data s)
         (Str.full_split pattern text))
  
  let () =
    List.iter
      (fun file ->
         let in_channel =
           if file = "-"
           then stdin
           else open_in file in
         let line = ref 0 in
         try
           while true do
             let text = input_line in_channel in
             incr line;
             print_endline (replace_all text !line file)
           done
         with End_of_file ->
           close_in in_channel)
      !args
  

