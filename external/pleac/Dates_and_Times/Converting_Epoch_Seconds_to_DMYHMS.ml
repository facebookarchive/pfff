(* ********************************************************************** *)
(* Converting Epoch Seconds to DMYHMS *)
(* ********************************************************************** *)
let pleac_Converting_Epoch_Seconds_to_DMYHMS () = 
  #load "unix.cma";;
  
  let time = Unix.time ()
  
  let {Unix.tm_sec=seconds; tm_min=minutes; tm_hour=hours;
       tm_mday=day_of_month; tm_mon=month; tm_year=year;
       tm_wday=wday; tm_yday=yday; tm_isdst=isdst} =
    Unix.localtime time
  
  let () =
    Printf.printf "Dateline: %02d:%02d:%02d-%04d/%02d/%02d\n"
      hours minutes seconds (year + 1900) (month + 1) day_of_month
  

