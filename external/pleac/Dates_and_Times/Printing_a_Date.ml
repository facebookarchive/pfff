(* ********************************************************************** *)
(* Printing a Date *)
(* ********************************************************************** *)
let pleac_Printing_a_Date () = 
  #load "unix.cma";;
  
  open Unix
  open Printf
  
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
  
  let time = fst (Unix.mktime {tm_sec=50; tm_min=45; tm_hour=3;
                               tm_mday=18; tm_mon=0; tm_year=73;
                               tm_wday=0; tm_yday=0; tm_isdst=false})
  let () = printf "format_time gives: %s\n" (format_time time)
  

