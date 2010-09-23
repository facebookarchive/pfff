(* ********************************************************************** *)
(* Parsing Dates and Times from Strings *)
(* ********************************************************************** *)
let pleac_Parsing_Dates_and_Times_from_Strings () = 
  #load "unix.cma";;
  
  let epoch_seconds date =
    Scanf.sscanf date "%04d-%02d-%02d"
      (fun yyyy mm dd ->
         fst (Unix.mktime {Unix.tm_sec=0; tm_min=0; tm_hour=0;
                           tm_mday=dd; tm_mon=mm-1; tm_year=yyyy-1900;
                           tm_wday=0; tm_yday=0; tm_isdst=false}))
  
  let () =
    while true do
      let line = read_line () in
      try
        let date = epoch_seconds line in
        let {Unix.tm_mday=day; tm_mon=month; tm_year=year} =
          Unix.localtime date in
        let month = month + 1 in
        let year = year + 1900 in
        Printf.printf "Date was %d/%d/%d\n" month day year
      with
        | Scanf.Scan_failure _
        | End_of_file 
        | Unix.Unix_error (Unix.ERANGE, "mktime", _) ->
            Printf.printf "Bad date string: %s\n" line
    done
  

