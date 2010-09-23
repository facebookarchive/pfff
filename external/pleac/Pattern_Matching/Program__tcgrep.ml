(* ********************************************************************** *)
(* Program: tcgrep *)
(* ********************************************************************** *)
let pleac_Program__tcgrep () = 
  % tcgrep -ril '^From: .*kate' ~/mail
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* tcgrep: rewrite of tom christiansen's rewrite of grep *)
  #load "unix.cma";;
  
  (* http://ocaml.info/home/ocaml_sources.html#pcre-ocaml *)
  #directory "+pcre";;
  #load "pcre.cma";;
  
  (* http://alain.frisch.fr/soft.html#Getopt *)
  #directory "+getopt";;
  #load "getopt.cma";;
  
  (* Initialize globals. *)
  let me = Pcre.replace ~pat:".*/" Sys.argv.(0)
  let matches = ref 0
  let errors = ref 0
  let grand_total = ref 0
  let mult = ref false
  let compress = [".z", "zcat"; ".gz", "zcat"; ".Z", "zcat"]
  
  (* Prints usage and exits. *)
  let usage () =
    Printf.eprintf "usage: %s [flags] [files]
  
  Standard grep options:
          i   case insensitive
          n   number lines
          c   give count of lines matching
          C   ditto, but >1 match per line possible
          w   word boundaries only
          s   silent mode
          x   exact matches only
          v   invert search sense (lines that DON'T match)
          h   hide filenames
          e   expression (for exprs beginning with -)
          f   file with expressions
          l   list filenames matching
  
  Specials:
          1   1 match per file
          H   highlight matches
          u   underline matches
          r   recursive on directories or dot if none
          t   process directories in 'ls -t' order
          p   paragraph mode (default: line mode)
          P   ditto, but specify separator, e.g. -P '%%'
          a   all files, not just plain text files (not implemented)
          q   quiet about failed file and dir opens
          T   trace files as opened
  
  May use a TCGREP environment variable to set default options.
  " me;
    exit 255
  
  (* Produces a stream of lines from an input channel. *)
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  (* Produces a stream of chunks from an input channel given a delimiter. *)
  let delimited_stream_of_channel delim channel =
    let lines = line_stream_of_channel channel in
    let rec next para_lines i =
      match Stream.peek lines, para_lines with
        | None, [] -> None
        | Some delim', [] when delim' = delim ->
            Stream.junk lines; next para_lines i
        | Some delim', _ when delim' = delim ->
            Some (String.concat "\n" (List.rev para_lines))
        | None, _ ->
            Some (String.concat "\n" (List.rev para_lines))
        | Some line, _ -> Stream.junk lines; next (line :: para_lines) i in
    Stream.from (next [])
  
  (* An empty delimiter corresponds to an empty line, so we can create
     a paragraph stream in terms of the previous function. *)
  let paragraph_stream_of_channel = delimited_stream_of_channel ""
  
  (* By default, the stream builder will produce lines. This can be changed
     by the -p and -P options. *)
  let stream_of_channel = ref line_stream_of_channel
  
  (* Type for command-line options and their values. *)
  type opt = OBool of bool | OStr of string
  
  (* Set an option. *)
  let opt_set opt c =
    Hashtbl.replace opt c (OBool true)
  
  (* Test an option. *)
  let opt_test opt c =
    try
      match Hashtbl.find opt c with
        | OBool b -> b
        | OStr "" -> false
        | OStr _ -> true
    with Not_found ->
      false
  
  (* Convert an option to a string. *)
  let opt_str opt c =
    try
      match Hashtbl.find opt c with
        | OBool b -> string_of_bool b
        | OStr s -> s
    with Not_found ->
      ""
  
  (* Gets terminal escape characters. *)
  let tput cap =
    let ch = Unix.open_process_in ("tput " ^ cap) in
    try
      let result = input_line ch in
      ignore (Unix.close_process_in ch);
      result
    with
      | End_of_file ->
          ignore (Unix.close_process_in ch);
          ""
      | e ->
          ignore (Unix.close_process_in ch);
          raise e
  
  (* Splits a filename into its base and extension. *)
  let splitext name =
    try
      let base = Filename.chop_extension name in
      let i = String.length base in
      let ext = String.sub name i (String.length name - i) in
      base, ext
    with Invalid_argument _ ->
      name, ""
  
  (* Parses command-line arguments. *)
  let parse_args () =
    let opt = Hashtbl.create 0 in
    let args = ref [] in
  
    let optstring = "incCwsxvhe:f:l1HurtpP:aqT" in
    let optstream = Stream.of_string optstring in
  
    (* Prepare options for Getopt. *)
    let opts =
      let str_setter c =
        (c, Getopt.nolong,
         None,
         Some (fun s -> Hashtbl.replace opt c (OStr s))) in
      let int_setter c =
        (c, Getopt.nolong,
         Some (fun () -> Hashtbl.replace opt c (OBool true)),
         None) in
      let rec loop acc =
        match Stream.peek optstream with
          | Some c ->
              (Stream.junk optstream;
               match Stream.peek optstream with
                 | Some ':' ->
                     Stream.junk optstream;
                     loop (str_setter c :: acc)
                 | _ ->
                     loop (int_setter c :: acc))
          | None -> List.rev acc in
      loop [] in
  
    (* Handle TCGREP environment variable. *)
    let cmdline = ref (List.tl (Array.to_list Sys.argv)) in
    Array.iter
      (fun env ->
         if (String.length env > 7
             && String.sub env 0 7 = "TCGREP=")
         then
           begin
             let s = String.sub env 7 (String.length env - 7) in
             let s = if s.[0] <> '-' then "-" ^ s else s in
             cmdline := s :: !cmdline
           end)
      (Unix.environment ());
    let cmdline = Array.of_list !cmdline in
  
    (* Parse command-line options using Getopt. *)
    begin
      try
        Getopt.parse
          opts (fun arg -> args := arg :: !args)
          cmdline 0 (Array.length cmdline - 1);
        args := List.rev !args
      with Getopt.Error e ->
        prerr_endline e;
        usage ()
    end;
  
    (* Read patterns from file or command line. *)
    let patterns =
      if opt_test opt 'f'
      then
        begin
          let in_channel =
            try open_in (opt_str opt 'f')
            with e ->
              Printf.eprintf "%s: can't open %s: %s\n"
                me (opt_str opt 'f') (Printexc.to_string e);
              exit 255 in
          try
            let acc = ref [] in
            Stream.iter
              (fun pat -> acc := pat :: !acc)
              (line_stream_of_channel in_channel);
            close_in in_channel;
            List.rev !acc
          with e ->
            close_in in_channel;
            Printf.eprintf "%s: error reading %s: %s\n"
              me (opt_str opt 'f') (Printexc.to_string e);
            exit 255
        end
      else if opt_test opt 'e'
      then [opt_str opt 'e']
      else [match !args with h :: t -> (args := t; h) | [] -> usage ()] in
  
    (* Terminal escape characters for highlighting options. *)
    let highlight =
      if opt_test opt 'H'
      then tput "smso" ^ "$1" ^ tput "rmso"
      else if opt_test opt 'u'
      then tput "smul" ^ "$1" ^ tput "rmul"
      else "$1" in
  
    (* Regular expression flags to use. *)
    let flags = ref [] in
    if opt_test opt 'i' then flags := `CASELESS :: !flags;
  
    (* Options for paragraph modes. *)
    if opt_test opt 'p'
    then stream_of_channel := paragraph_stream_of_channel;
    if opt_test opt 'P'
    then stream_of_channel := delimited_stream_of_channel (opt_str opt 'P');
  
    (* Word boundary option. *)
    let patterns =
      if opt_test opt 'w'
      then List.map (fun pat -> "\\b" ^ pat ^ "\\b") patterns
      else patterns in
  
    (* Exact match option. *)
    let patterns =
      if opt_test opt 'x'
      then List.map (fun pat -> "^" ^ pat ^ "$") patterns
      else patterns in
  
    (* Options that imply other options. *)
    if opt_test opt 'l' then opt_set opt '1';
    if opt_test opt 'u' then opt_set opt 'H';
    if opt_test opt 'C' then opt_set opt 'c';
    if opt_test opt 'c' then opt_set opt 's';
    if opt_test opt 's' && not (opt_test opt 'c') then opt_set opt '1';
  
    (* Compile the regular expression patterns. *)
    let rexes =
      List.map
        (fun pat ->
           try Pcre.regexp ~flags:!flags ("(" ^ pat ^ ")")
           with Pcre.BadPattern (msg, _) ->
             Printf.eprintf "%s: bad pattern %s: %s\n" me pat msg;
             exit 255)
        patterns in
  
    (* Increments the matches variable by the number of matches
       (or non-matches) in the given line. *)
    let count_matches line =
      if opt_test opt 'v'
      then fun rex ->
        (if not (Pcre.pmatch ~rex line) then incr matches)
      else if opt_test opt 'C'
      then fun rex ->
        (matches := !matches + (try Array.length (Pcre.extract_all ~rex line)
                                with Not_found -> 0))
      else fun rex ->
        (if Pcre.pmatch ~rex line then incr matches) in
  
    (* Counts matches in a line and returns the line with any
       necessary highlighting. *)
    let matcher line =
      List.iter (count_matches line) rexes;
      if opt_test opt 'H'
      then
        List.fold_left
          (fun line rex ->
             Pcre.replace ~rex ~templ:highlight line)
          line rexes
      else line in
  
    (* List of files or directories to process. *)
    let files =
      match !args with
        | [] -> if opt_test opt 'r' then ["."] else ["-"]
        | [arg] -> [arg]
        | args -> (mult := true; args) in
  
    (* Overrides for options that affect the multiple-file flag. *)
    if opt_test opt 'h' then mult := false;
    if opt_test opt 'r' then mult := true;
  
    (* Return the three values to be processed by matchfiles. *)
    opt, matcher, files
  
  (* Used to break out of loops and abort processing of the current file. *)
  exception NextFile
  
  (* Runs given matcher on a list of files using the specified options. *)
  let rec matchfiles opt matcher files =
    (* Handles a single directory. *)
    let matchdir dir =
      if not (opt_test opt 'r')
      then
        begin
          if opt_test opt 'T'
          then (Printf.eprintf "%s: \"%s\" is a directory, but no -r given\n"
                  me dir;
                flush stderr)
        end
      else
        begin
          let files =
            try Some (Sys.readdir dir)
            with e ->
              if not (opt_test opt 'q')
              then (Printf.eprintf "%s: can't readdir %s: %s\n"
                      me dir (Printexc.to_string e);
                    flush stderr);
              incr errors;
              None in
          match files with
            | Some files ->
                let by_mtime a b =
                  compare
                    (Unix.stat (Filename.concat dir b)).Unix.st_mtime
                    (Unix.stat (Filename.concat dir a)).Unix.st_mtime in
                if opt_test opt 't'
                then Array.sort by_mtime files;
                matchfiles opt matcher
                  (Array.to_list
                     (Array.map
                        (Filename.concat dir)
                        files))
            | None -> ()
        end in
  
    (* Handles a single file. *)
    let matchfile file =
      (* Keep a running total of matches for this file. *)
      let total = ref 0 in
  
      (* Keep track of the current line number. *)
      let line_num = ref 0 in
  
      (* Shadow close_in to properly close process channels for compressed
         files and avoid closing stdin. *)
      let process_open = ref false in
      let close_in channel =
        if !process_open
        then ignore (Unix.close_process_in channel)
        else if channel != stdin then close_in channel in
  
      (* Process a line (or paragraph, with -p or -P) of input. *)
      let matchline line =
         incr line_num;
         matches := 0;
         let line = matcher line in
         if !matches > 0
         then
           begin
             total := !total + !matches;
             grand_total := !grand_total + !matches;
             if opt_test opt 'l'
             then (print_endline file; raise NextFile)
             else if not (opt_test opt 's')
             then (Printf.printf "%s%s%s%s\n"
                     (if !mult
                      then file ^ ":"
                      else "")
                     (if opt_test opt 'n'
                      then (string_of_int !line_num
                            ^ (if opt_test opt 'p' || opt_test opt 'P'
                               then ":\n"
                               else ":"))
                      else "")
                     line
                     (if opt_test opt 'p' || opt_test opt 'P'
                      then "\n" ^ String.make 20 '-'
                      else "");
                   flush stdout);
             if opt_test opt '1' then raise NextFile
           end in
  
      (* Get a channel for the file, starting a decompression process if
         necessary. If None, the file will be skipped. *)
      let maybe_channel =
        if file = "-"
        then (if Unix.isatty Unix.stdin && not (opt_test opt 'q')
              then (Printf.eprintf "%s: reading from stdin\n" me;
                    flush stderr);
              Some stdin)
        else if not (Sys.file_exists file)
        then (if not (opt_test opt 'q')
              then (Printf.eprintf "%s: file \"%s\" does not exist\n"
                      me file;
                    flush stderr);
              incr errors;
              None)
        else if List.mem_assoc (snd (splitext file)) compress
        then (process_open := true;
              try Some (Unix.open_process_in
                          (List.assoc (snd (splitext file)) compress
                           ^ " < " ^ file))
              with e ->
                if not (opt_test opt 'q')
                then (Printf.eprintf "%s: %s: %s\n" me file
                        (Printexc.to_string e);
                      flush stderr);
                incr errors;
                None)
        else (try Some (open_in file)
              with e ->
                if not (opt_test opt 'q')
                then (Printf.eprintf "%s: %s: %s\n" me file
                        (Printexc.to_string e);
                      flush stderr);
                incr errors;
                None) in
  
      (* Run matcher on the open channel, then close the channel. *)
      match maybe_channel with
        | None -> ()
        | Some channel ->
            begin
              try
                if opt_test opt 'T'
                then (Printf.eprintf "%s: checking %s\n" me file;
                      flush stderr);
                Stream.iter matchline (!stream_of_channel channel);
                close_in channel
              with
                | NextFile ->
                    close_in channel
                | e ->
                    close_in channel;
                    raise e
            end;
            if opt_test opt 'c'
            then (Printf.printf "%s%d\n"
                    (if !mult then file ^ ":" else "")
                    !total;
                  flush stdout) in
  
    (* Handle each of the specified files and directories. *)
    List.iter
      (fun file ->
         if file = "-"
         then matchfile file
         else if try Sys.is_directory file with _ -> false
         then matchdir file
         else matchfile file)
      files
  
  (* Parse command line arguments, run matcher, and set result status. *)
  let opt, matcher, files = parse_args ()
  let () =
    matchfiles opt matcher files;
    if !errors > 0 then exit 2;
    if !grand_total > 0 then exit 0;
    exit 1
  

