(* ********************************************************************** *)
(* Processing Server Logs *)
(* ********************************************************************** *)
let pleac_Processing_Server_Logs () = 
  #!/usr/bin/ocaml
  (* sumwww - summarize web server log activity *)
  
  #use "topfind";;
  #require "weblogs";;
  
  open Weblogs
  
  let file =
    if Array.length Sys.argv = 2
    then Sys.argv.(1)
    else (Printf.eprintf "usage: %s <logfile>\n" Sys.argv.(0);
          exit 1)
  
  let format_date = CalendarLib.Printer.CalendarPrinter.sprint "%d/%b/%Y"
  
  let incr_hash hash key by =
    Hashtbl.replace hash key
      (try Hashtbl.find hash key + by
       with Not_found -> by)
  
  let count_hash hash =
    let count = ref 0 in
    Hashtbl.iter (fun _ _ -> incr count) hash;
    !count
  
  let add_hash dest src =
    Hashtbl.iter (incr_hash dest) src
  
  let lastdate = ref ""
  
  let count = ref 0
  let posts = ref 0
  let homes = ref 0
  let bytesum = ref 0l
  let hosts = ref (Hashtbl.create 0)
  let whats = ref (Hashtbl.create 0)
  
  let sumcount = ref 0
  let allposts = ref 0
  let allhomes = ref 0
  let bytesumsum = ref 0l
  let allhosts = ref (Hashtbl.create 0)
  let allwhats = ref (Hashtbl.create 0)
  
  (* display the tallies of hosts and URLs *)
  let write_report () =
    Printf.printf "%s %7d %8d %8d %7d %7d %14ld\n%!"
      !lastdate (count_hash !hosts) !count (count_hash !whats)
      !posts !homes !bytesum;
  
    (* add to summary data *)
    sumcount := !sumcount + !count;
    bytesumsum := Int32.add !bytesumsum !bytesum;
    allposts := !allposts + !posts;
    allhomes := !allhomes + !homes;
  
    (* reset daily data *)
    count := 0;
    posts := 0;
    homes := 0;
    bytesum := 0l;
    add_hash !allhosts !hosts;
    add_hash !allwhats !whats;
    Hashtbl.clear !hosts;
    Hashtbl.clear !whats
  
  (* read log file and tally hits from the host and to the URL *)
  let daily_logs () =
    let log = import_file file in
    print_endline
      "    Date     Hosts  Accesses  Unidocs   POST    Home       Bytes";
    print_endline
      "----------- ------- -------- -------- ------- ------- --------------";
    Array.iter
      (fun row ->
         let date = format_date row.t in
         let host = row.src_ip in
         let what = row.url in
         let post = row.http_method = POST in
         let home = what = "/" in
         let bytes = match row.size with Some n -> n | None -> 0 in
         if !lastdate = "" then lastdate := date;
         if !lastdate <> date then write_report ();
         lastdate := date;
         incr count;
         if post then incr posts;
         if home then incr homes;
         incr_hash !hosts host 1;
         incr_hash !whats what 1;
         bytesum := Int32.add !bytesum (Int32.of_int bytes))
      log;
    if !count > 0 then write_report ()
  
  let summary () =
    lastdate := "Grand Total";
    count := !sumcount;
    bytesum := !bytesumsum;
    hosts := !allhosts;
    posts := !allposts;
    whats := !allwhats;
    homes := !allhomes;
    write_report ()
  
  let () =
    daily_logs ();
    summary ();
    exit 0
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* aprept - report on Apache logs *)
  
  #use "topfind";;
  #require "weblogs";;
  
  open Weblogs
  
  let file =
    if Array.length Sys.argv = 2
    then Sys.argv.(1)
    else (Printf.eprintf "usage: %s <logfile>\n" Sys.argv.(0);
          exit 1)
  
  let log = import_file file
  let conn = HostIP.connection ()
  
  let incr_hash hash key by =
    Hashtbl.replace hash key
      (try Hashtbl.find hash key + by
       with Not_found -> by)
  
  let report_countries () =
    let total = ref 0 in
    let countries = Hashtbl.create 0 in
    Array.iter
      (fun row ->
         let country =
           match HostIP.get_country_name conn row.src_ip with
             | Some country -> country
             | None -> "UNKNOWN" in
         incr_hash countries country 1;
         incr total)
      log;
    let country_records = ref [] in
    Hashtbl.iter
      (fun country count ->
         country_records := (count, country) :: !country_records)
      countries;
    print_endline "Domain                  Records";
    print_endline "===============================";
    List.iter
      (fun (count, country) ->
         Printf.printf "%18s %5d %5.2f%%\n%!"
           country count (float count *. 100. /. float !total))
      (List.rev (List.sort compare !country_records))
  
  let report_files () =
    let total = ref 0 in
    let totalbytes = ref 0l in
    let bytes = Hashtbl.create 0 in
    let records = Hashtbl.create 0 in
    Array.iter
      (fun row ->
         let file = row.url in
         let size = match row.size with Some n -> n | None -> 0 in
         incr_hash bytes file size;
         incr_hash records file 1;
         totalbytes := Int32.add !totalbytes (Int32.of_int size);
         incr total)
      log;
    let file_records = ref [] in
    Hashtbl.iter
      (fun file size ->
         let count = Hashtbl.find records file in
         file_records := (file, size, count) :: !file_records)
      bytes;
    print_endline
      "File                               Bytes          Records";
    print_endline
      "=========================================================";
    List.iter
      (fun (file, size, count) ->
         Printf.printf "%-22s %10d %5.2f%% %9d %5.2f%%\n%!"
           file size
           (float size *. 100. /. Int32.to_float !totalbytes)
           count
           (float count *. 100. /. float !total))
      (List.sort compare !file_records)
  
  let () =
    report_countries ();
    print_newline ();
    report_files ()
  

