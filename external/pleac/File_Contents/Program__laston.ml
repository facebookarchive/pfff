(* ********************************************************************** *)
(* Program: laston *)
(* ********************************************************************** *)
let pleac_Program__laston () = 
  (* laston - find out when a given user last logged on *)
  
  #load "str.cma";;
  #load "unix.cma";;
  
  open Printf
  open Unix
  
  let lastlog = open_in "/var/log/lastlog"
  let sizeof = 4 + 12 + 16
  let line = String.make 12 ' '
  let host = String.make 16 ' '
  
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                  "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  
  let format_time time =
    let tm = localtime time in
    sprintf "%s %s %2d %02d:%02d:%02d %04d"
      days.(tm.tm_wday)
      months.(tm.tm_mon)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
      (tm.tm_year + 1900)
  
  let trim_asciiz s =
    try String.sub s 0 (String.index s '\000')
    with Not_found -> s
  
  let () =
    Array.iter
      (fun user ->
         try
           let u =
             try getpwuid (int_of_string user)
             with Failure _ -> getpwnam user in
           seek_in lastlog (u.pw_uid * sizeof);
           let time = input_binary_int lastlog in
           really_input lastlog line 0 12;
           really_input lastlog host 0 16;
           let line = trim_asciiz line in
           let host = trim_asciiz host in
           printf "%-8s UID %5d %s%s%s\n"
             u.pw_name
             u.pw_uid
             (if time <> 0
              then format_time (float_of_int time)
              else "never logged in")
             (if line <> "" then " on " ^ line else "")
             (if host <> "" then " from " ^ host else "")
         with Not_found ->
           printf "no such uid %s\n" user)
      (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  
  

