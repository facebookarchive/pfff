(* ********************************************************************** *)
(* Program: ggh - Grep Netscape Global History *)
(* ********************************************************************** *)
let pleac_Program__ggh___Grep_Netscape_Global_History () = 
  (* Search the history using the Places SQLite database, new in Firefox 3.
     Pattern-matching uses simple substrings, but it could be expanded to use
     Str or Pcre by installing a user-defined function. *)
  
  #directory "+sqlite3";;
  #load "sqlite3.cma";;
  #load "unix.cma";;
  
  type history = { visit_date : Unix.tm;
                   url        : string;
                   title      : string; }
  
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                  "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  
  let string_of_tm tm =
    Printf.sprintf "%s %s %2d %02d:%02d:%02d %04d"
      days.(tm.Unix.tm_wday)
      months.(tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec
      (tm.Unix.tm_year + 1900)
  
  let tm_of_micros micros =
    let time = float_of_string micros /. 1000000. in
    Unix.localtime time
  
  let () =
    if Array.length Sys.argv < 2 then
      begin
        Printf.printf "Usage: %s path/to/places.sqlite [pattern]\n"
          Sys.argv.(0);
        exit 0
      end
  
  let file =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "places.sqlite"
  
  let pattern =
    if Array.length Sys.argv > 2 then Some Sys.argv.(2) else None
  
  let db = Sqlite3.db_open file
  
  let sql =
    Printf.sprintf
      "SELECT   visit_date, url, title
       FROM     moz_places p
       JOIN     moz_historyvisits v
       ON       p.id = v.place_id
       %s
       ORDER BY visit_date DESC"
      (match pattern with
         | None -> ""
         | Some s ->
             (Printf.sprintf "WHERE url LIKE '%%%s%%' OR title LIKE '%%%s%%'"
                s s))
  
  let data = ref []
  
  let res =
    Sqlite3.exec_not_null_no_headers db
      ~cb:(fun row ->
             data := {visit_date = tm_of_micros row.(0);
                      url = row.(1);
                      title = row.(2)} :: !data) sql
  
  let () =
    match res with
      | Sqlite3.Rc.OK ->
          List.iter
            (fun history ->
               Printf.printf "[%s] %s \"%s\"\n"
                 (string_of_tm history.visit_date)
                 history.url
                 history.title)
            !data
      | r ->
          Printf.eprintf "%s: %s\n"
            (Sqlite3.Rc.to_string r)
            (Sqlite3.errmsg db)
  
  

