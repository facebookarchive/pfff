(* ********************************************************************** *)
(* Program: sigrand *)
(* ********************************************************************** *)
let pleac_Program__sigrand () = 
  #!/usr/bin/ocaml
  (* sigrand - supply random fortunes for .signature file *)
  #load "str.cma";;
  #load "unix.cma";;
  
  (* globals *)
  
  let pwd = Unix.getpwuid (Unix.getuid ())
  
  let home =
    try Unix.getenv "HOME" with Not_found ->
      try Unix.getenv "LOGDIR" with Not_found ->
        pwd.Unix.pw_dir
  
  let fortune_path = ref ""
  
  (**************************************************************)
  (* begin configuration section *)
  
  (* for rec/humor/funny instead of rec.humor.funny *)
  let ng_is_dir      = true
  
  let fullname       = home ^ "/.fullname"
  let fifo           = home ^ "/.signature"
  let art            = home ^ "/.article"
  let news           = home ^ "/News"
  let sigs           = news ^ "/SIGNATURES"
  let sema           = home ^ "/.sigrandpid"
  let globrand       = 0.25  (* chance to use global sigs anyway *)
  
  (* name should be (1) left None to have program guess
     read address for signature maybe looking in ~/.fullname,
     (2) set to an exact address, or (3) set to empty string
     to be omitted entirely. *)
  
  (* let name        = ref None *)
  (* let name        = ref (Some ("me@home.org")) *)
  let name           = ref (Some "")
  
  (* end configuration section *)
  (**************************************************************)
  
  let read_process_lines command =
    let lines = ref [] in
    let in_channel = Unix.open_process_in command in
    begin
      try
        while true do
          lines := input_line in_channel :: !lines
        done;
      with End_of_file ->
        ignore (Unix.close_process_in in_channel)
    end;
    List.rev !lines
  
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
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
  
  (* Make sure there's a fortune program.  Search
     for its full path and set global to that. *)
  let check_fortunes () =
    if !fortune_path <> ""
    then ()  (* already set *)
    else
      let path = Str.split (Str.regexp ":") (Unix.getenv "PATH") in
      let rec check = function
        | [] ->
            Printf.eprintf
              "Need either %s or a fortune program, bailing out\n"
              sigs;
            exit 1
        | dir :: dirs ->
            let p = Filename.concat dir "fortune" in
            if Sys.file_exists p then p else check dirs in
      fortune_path := check (path @ ["/usr/games"])
  
  (* Call the fortune program with -s for short flag until
     we get a small enough fortune or ask too much. *)
  let fortune () =
    let cmd = !fortune_path ^ " -s" in
    let rec loop tries =
      let lines = read_process_lines cmd in
      if List.length lines < 5 then lines
      else if tries < 20 then loop (tries + 1)
      else [] in
    match loop 0 with
      | [] ->
          [" SIGRAND: deliver random signals to all processes."]
      | lines ->
          List.map (( ^ ) " ") lines
  
  (* See whether ~/.article contains a Newsgroups line. if so, see the
     first group posted to and find out whether it has a dedicated set of
     fortunes. otherwise return the global one. Also, return the global
     one randomly now and then to spice up the sigs. *)
  let signame () =
    if Random.float 1.0 > globrand
    then
      begin
        try
          let channel = open_in art in
          let regexp = Str.regexp "Newsgroups:[ \t]*\\([^, \r\n\t]*\\)" in
          let ng = ref "" in
          begin
            try
              while true do
                let line = input_line channel in
                if Str.string_match regexp line 0
                then ng := Str.matched_group 1 line
              done
            with End_of_file ->
              close_in channel
          end;
          if ng_is_dir
          then ng := Str.global_replace (Str.regexp "\\.") "/" !ng;
          ng := news ^ "/" ^ !ng ^ "/" ^ "SIGNATURES";
          if Sys.file_exists !ng then !ng else sigs
        with Sys_error e ->
          sigs
      end
    else sigs
  
  (* choose a random signature *)
  let pick_quote () =
    let sigfile = signame () in
    if not (Sys.file_exists sigfile)
    then fortune ()
    else
      begin
        let channel = open_in sigfile in
        let stream = delimited_stream_of_channel "%%" channel in
        let quip = ref [] in
        let num = ref 1 in
        Stream.iter
          (fun chunk ->
             if Random.int !num = 0
             then quip := Str.split (Str.regexp "\n") chunk;
             incr num)
          stream;
        close_in channel;
        if !quip <> []
        then List.map (( ^ ) " ") !quip
        else [" ENOSIG: This signature file is empty."]
      end
  
  (* Ignore SIGPIPE in case someone opens us up and then closes the fifo
     without reading it; look in a .fullname file for their login name.
     Try to determine the fully qualified hostname. Make sure we have
     signatures or fortunes. Build a fifo if we need to. *)
  
  let setup () =
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  
    if !name = Some "" then
      begin
        try
          let channel = open_in fullname in
          name := Some (input_line channel);
          close_in channel
        with Sys_error _ ->
          name := Some (Str.global_replace (Str.regexp ",.*") ""
                          pwd.Unix.pw_gecos)
      end;
  
    if not (Sys.file_exists sigs) then check_fortunes ();
  
    if Sys.file_exists fifo
    then (if (Unix.stat fifo).Unix.st_kind = Unix.S_FIFO
          then (Printf.eprintf "%s: using existing named pipe %s\n"
                  Sys.argv.(0) fifo)
          else (Printf.eprintf "%s: won't overwrite file %s\n"
                  Sys.argv.(0) fifo;
                exit 1))
    else (Unix.mkfifo fifo 0o666;
          Printf.eprintf "%s: created %s as a named pipe\n"
            Sys.argv.(0) fifo);
  
    Random.self_init ()
  
  (* "There can be only one."  --the Highlander *)
  let justme () =
    let channel =
      try Some (open_in sema)
      with Sys_error _ -> None in
    match channel with
      | Some channel ->
          begin
            let pid = int_of_string (input_line channel) in
            try
              Unix.kill pid 0;
              Printf.eprintf "%s already running (pid %d), bailing out\n"
                Sys.argv.(0) pid;
              exit 1
            with _ ->
              close_in channel
          end
      | None -> ()
  
  let () =
    setup ();                (* pull in inits *)
    justme ();               (* make sure program not already running *)
    match Unix.fork () with  (* background ourself and go away *)
      | 0 ->
          let channel = open_out sema in
          output_string channel (string_of_int (Unix.getpid ()));
          output_string channel "\n";
          close_out channel;
  
          (* now loop forever, writing a signature into the
             fifo file.  if you don't have real fifos, change
             sleep time at bottom of loop to like 10 to update
             only every 10 seconds. *)
  
          while true do
            let channel = open_out fifo in
            let sig' = pick_quote () in
            let sig' = Array.of_list sig' in
  
            (* trunc to 4 lines *)
            let sig' =
              if Array.length sig' > 4
              then Array.sub sig' 0 4
              else sig' in
  
            (* trunc long lines *)
            let sig' =
              Array.map
                (fun line ->
                   if String.length line > 80
                   then String.sub line 0 80
                   else line)
                sig' in
  
            (* print sig, with name if present, padded to four lines *)
            begin
              match !name with
                | None | Some "" ->
                    Array.iter
                      (fun line ->
                         output_string channel line;
                         output_string channel "\n")
                      sig'
                | Some name ->
                    output_string channel name;
                    for i = 4 downto Array.length sig' do
                      output_string channel "\n";
                    done;
                    Array.iter
                      (fun line ->
                         output_string channel line;
                         output_string channel "\n")
                      sig'
            end;
            close_out channel;
  
            (* Without a microsleep, the reading process doesn't finish
               before the writer tries to open it again, which since the
               reader exists, succeeds. They end up with multiple
               signatures. Sleep a tiny bit between opens to give readers
               a chance to finish reading and close our pipe so we can
               block when opening it the next time. *)
  
            ignore (Unix.select [] [] [] 0.2)  (* sleep 1/5 second *)
          done
      | _ ->
          exit 0
  
  

