(* ********************************************************************** *)
(* Day in a Week/Month/Year or Week Number *)
(* ********************************************************************** *)
let pleac_Day_in_a_Week_Month_Year_or_Week_Number () = 
  #load "unix.cma";;
  
  let {Unix.tm_mday=monthday; tm_wday=weekday; tm_yday=yearday} =
    Unix.localtime date
  let weeknum = yearday / 7 + 1
  

