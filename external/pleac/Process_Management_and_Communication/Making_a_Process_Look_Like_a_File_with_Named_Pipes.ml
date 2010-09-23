(* ********************************************************************** *)
(* Making a Process Look Like a File with Named Pipes *)
(* ********************************************************************** *)
let pleac_Making_a_Process_Look_Like_a_File_with_Named_Pipes () = 
  % mkfifo /path/to/named.pipe
  
  (*-----------------------------*)
  
  let () =
    let fifo = open_in "/path/to/named.pipe" in
    try
      while true do
        let line = input_line fifo in
        Printf.printf "Got: %s\n" line
      done
    with End_of_file ->
      close_in fifo
  
  (*-----------------------------*)
  
  let () =
    let fifo = open_out "/path/to/named.pipe" in
    output_string fifo "Smoke this.\n";
    close_out fifo
  
  (*-----------------------------*)
  
  % mkfifo ~/.plan                    # isn't this everywhere yet?
  % mknod  ~/.plan p                  # in case you don't have mkfifo
  
  (*-----------------------------*)
  
  (* dateplan - place current date and time in .plan file *)
  #load "unix.cma";;
  let () =
    while true do
      let home = Unix.getenv "HOME" in
      let fifo = open_out (home ^ "/.plan") in
      Printf.fprintf fifo "The current time is %s\n"
        (format_time (Unix.time ()));
      close_out fifo;
      Unix.sleep 1
    done
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* fifolog - read and record log msgs from fifo *)
  #load "unix.cma";;
  
  let fifo = ref None
  
  let handle_alarm signal =
    match !fifo with
      | Some channel ->
          (* move on to the next queued process *)
          close_in channel;
          fifo := None
      | None -> ()
  
  let () =
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle handle_alarm)
  
  let read_fifo () =
    try
      match !fifo with
        | Some channel -> Some (input_line channel)
        | None -> None
    with
      | End_of_file ->
          None
      | Sys_error e ->
          Printf.eprintf "Error reading fifo: %s\n%!" e;
          fifo := None;
          None
  
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                  "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  
  let format_time time =
    let tm = Unix.localtime time in
    Printf.sprintf "%s %s %2d %02d:%02d:%02d %04d"
      days.(tm.Unix.tm_wday)
      months.(tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec
      (tm.Unix.tm_year + 1900)
  
  let () =
    while true do
      (* turn off alarm for blocking open *)
      ignore (Unix.alarm 0);
      begin
        try fifo := Some (open_in "/tmp/log")
        with Sys_error e ->
          Printf.eprintf "Can't open /tmp/log: %s\n%!" e;
          exit 1
      end;
  
      (* you have 1 second to log *)
      ignore (Unix.alarm 1);
  
      let service = read_fifo () in
      let message = read_fifo () in
  
      (* turn off alarms for message processing *)
      ignore (Unix.alarm 0);
  
      begin
        match service, message with
          | None, _ | _, None ->
              (* interrupted or nothing logged *)
              ()
          | Some service, Some message ->
              if service = "http"
              then () (* ignoring *)
              else if service = "login"
              then
                begin
                  (* log to /tmp/login *)
                  try
                    let log =
                      open_out_gen
                        [Open_wronly; Open_creat; Open_append]
                        0o666
                        "/tmp/login" in
                    Printf.fprintf log "%s %s %s\n%!"
                      (format_time (Unix.time ())) service message;
                    close_out log
                  with Sys_error e ->
                    Printf.eprintf "Couldn't log %s %s to /tmp/login: %s\n%!"
                      service message e
                end
      end
    done
  

