(* ********************************************************************** *)
(* Managing the Screen *)
(* ********************************************************************** *)
let pleac_Managing_the_Screen () = 
  #!/usr/bin/ocaml
  (* rep - screen repeat command *)
  
  #load "unix.cma";;
  
  (* http://www.nongnu.org/ocaml-tmk/ *)
  #directory "+curses";;
  #load "curses.cma";;
  
  let timeout = 10.0
  
  let (timeout, command) =
    match Array.length Sys.argv with
      | 0 | 1 -> (timeout, [| |])
      | len ->
          if Sys.argv.(1) <> "" && Sys.argv.(1).[0] = '-'
          then (float_of_string
                  (String.sub Sys.argv.(1)
                     1 (String.length Sys.argv.(1) - 1)),
                Array.sub Sys.argv 2 (len - 2))
          else (timeout, Array.sub Sys.argv 1 (len - 1))
  
  let () =
    if Array.length command = 0
    then (Printf.printf "usage: %s [ -timeout ] cmd args\n" Sys.argv.(0);
          exit 255)
  
  let window = Curses.initscr ()          (* start screen *)
  let _ = Curses.noecho ()
  let _ = Curses.cbreak ()
  let _ = Curses.nodelay window true      (* so getch() is non-blocking *)
  
  let done' s _ = Curses.endwin (); print_endline s; exit 0
  let () = Sys.set_signal Sys.sigint (Sys.Signal_handle (done' "Ouch!"))
  
  let cols, lines = Curses.getmaxyx window
  
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
  
  let time = fst (Unix.mktime {Unix.tm_sec=50; tm_min=45; tm_hour=3;
                               tm_mday=18; tm_mon=0; tm_year=73;
                               tm_wday=0; tm_yday=0; tm_isdst=false})
  
  let () =
    while true do
      let key = ref (-1) in
      while key := Curses.getch (); !key <> -1 do
        if !key = Char.code 'q' then done' "See ya" ()
      done;
  
      let in_channel =
        Unix.open_process_in (String.concat " " (Array.to_list command)) in
      begin
        try
          for i = 0 to lines - 1 do
            let line = input_line in_channel in
            ignore (Curses.mvaddstr i 0 line);
  
            Curses.standout ();
            ignore (Curses.mvaddstr (lines - 1) (cols - 24)
                      (format_time (Unix.time ())));
            Curses.standend ();
  
            ignore (Curses.move 0 0);
            ignore (Curses.refresh ());
          done;
          ignore (Unix.close_process_in in_channel)
        with End_of_file ->
          ignore (Unix.close_process_in in_channel)
      end;
  
      ignore (Unix.select [Unix.stdin] [] [] timeout)
    done
  
  (*-----------------------------*)
  
  let err = Curses.keypad window true     (* enable keypad mode *)
  let key = Curses.getch ()
  let () =
    if (key = (Char.code 'k') ||          (* vi mode *)
        key = 16 ||                       (* emacs mode *)
        key = Curses.Key.up)              (* arrow mode *)
    then
      begin
        (* do something *)
      end
  

