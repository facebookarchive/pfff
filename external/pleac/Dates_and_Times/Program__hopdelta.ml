(* ********************************************************************** *)
(* Program: hopdelta *)
(* ********************************************************************** *)
let pleac_Program__hopdelta () = 
  #!/usr/bin/ocaml
  (* hopdelta - feed mail header, produce lines
                showing delay at each hop. *)
  #load "str.cma";;
  #load "unix.cma";;
  
  (* Modify this function to tweak the format of results. *)
  let print_result sender recipient time delta =
    Printf.printf "%-30s %-30s %-20s   %s\n"
      sender recipient time delta
  
  (* Produce a stream of lines from an input channel. *)
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  (* Turn a stream of lines into a stream of paragraphs, where each
     paragraph is a stream of lines. Paragraphs are delimited by one
     or more empty lines. *)
  let paragraphs lines =
    let rec next para_lines i =
      match Stream.peek lines, para_lines with
        | None, [] -> None
        | Some "", [] -> Stream.junk lines; next para_lines i
        | Some "", _ | None, _ -> Some (Stream.of_list (List.rev para_lines))
        | Some line, _ -> Stream.junk lines; next (line :: para_lines) i in
    Stream.from (next [])
  
  (* Find blocks of email headers in a stream of paragraphs. Headers
     are all assumed to have a first line starting with "From" and
     containing a '@' character. This is not very robust. *)
  let header_blocks paras =
    let rec next i =
      match Stream.peek paras with
        | Some lines ->
            if (match Stream.peek lines with
                  | Some line ->
                      (String.length line >= 5
                       && (String.sub line 0 5 = "From ")
                       && (String.contains line '@'))
                  | None -> false)
            then Some (Stream.next paras)
            else (Stream.junk paras; next i)
        | None -> None in
    Stream.from next
  
  (* Pattern to detect continuation lines. *)
  let continuation_regexp = Str.regexp "^[\t ]+"
  
  (* Transform a stream of lines such that continuation lines are joined
     with previous lines by a single space. *)
  let join_continuations lines =
    let rec continuations () =
      match Stream.peek lines with
        | Some line ->
            let found = ref false in
            let trimmed =
              Str.substitute_first
                continuation_regexp
                (fun _ -> found := true; "")
                line in
            if !found
            then (Stream.junk lines; " " ^ trimmed ^ continuations ())
            else ""
        | None -> "" in
    let rec next i =
      match Stream.peek lines with
        | Some line ->
            Stream.junk lines;
            Some (line ^ continuations ())
        | None -> None in
    Stream.from next
  
  (* A type for headers, where "from" contains the text of the "From"
     line, and the rest of the headers are parsed into a (key, value)
     list called "params". *)
  type header = { from : string;
                  params : (string * string) list }
  
  (* Given a stream of header blocks, produce a stream of values of the
     above "header" type. *)
  let headers blocks =
    let parse_from line =
      String.sub line 5 (String.length line - 5) in
    let parse_param params line =
      try
        let index = String.index line ':' in
        let key = String.sub line 0 index in
        let value =
          if String.length line > index + 2
          then
            String.sub
              line
              (index + 2)
              (String.length line - index - 2)
          else "" in
        params := (key, value) :: !params
      with
        | Not_found
        | Invalid_argument "String.sub" ->
            Printf.eprintf "Unable to parse header: %s\n" line;
            () in
    let rec next i =
      try
        let lines = Stream.next blocks in
        let lines = join_continuations lines in
        let from = parse_from (Stream.next lines) in
        let params = ref [] in
        Stream.iter (parse_param params) lines;
        Some { from = from; params = List.rev !params }
      with Stream.Failure ->
        None in
    Stream.from next
  
  (* Combine the above stream transformers to produce a function from
     input channels to streams of headers. *)
  let header_stream_of_channel channel =
    headers
      (header_blocks
         (paragraphs
            (line_stream_of_channel channel)))
  
  (* Association list mapping month abbreviations to 0-based month
     numbers as required by Unix.mktime. *)
  let months =
    ["Jan", 0; "Feb", 1; "Mar", 2; "Apr", 3; "May", 4; "Jun", 5;
     "Jul", 6; "Aug", 7; "Sep", 8; "Oct", 9; "Nov", 10; "Dec", 11]
  
  (* Turn a time zone into an offset in minutes. Not exhaustive. *)
  let parse_tz = function
    | "" | "Z" | "GMT" | "UTC" | "UT" -> 0
    | "PST" -> -480
    | "MST" | "PDT" -> -420
    | "CST" | "MDT" -> -360
    | "EST" | "CDT" -> -300
    | "EDT" -> -240
    | string ->
        Scanf.sscanf string "%c%02d%_[:]%02d"
          (fun sign hour min ->
             min + hour * (if sign = '-' then -60 else 60))
  
  (* List of date-parsing functions from strings to epoch seconds. *)
  let date_parsers =
    [
      (fun string ->
         Scanf.sscanf string "%d %s %d %d:%d:%d %s"
           (fun mday mon year hour min sec tz ->
              let mon = List.assoc mon months in
              fst (Unix.mktime
                     {Unix.tm_sec=sec; tm_min=min; tm_hour=hour;
                      tm_mday=mday; tm_mon=mon; tm_year=year-1900;
                      tm_wday=0; tm_yday=0; tm_isdst=false})
              -. (float (parse_tz tz) *. 60.0)));
      (fun string ->
         Scanf.sscanf string "%3s, %d %s %4d %d:%d:%d %s"
           (fun wday mday mon year hour min sec tz ->
              let mon = List.assoc mon months in
              fst (Unix.mktime
                     {Unix.tm_sec=sec; tm_min=min; tm_hour=hour;
                      tm_mday=mday; tm_mon=mon; tm_year=year-1900;
                      tm_wday=0; tm_yday=0; tm_isdst=false})
              -. (float (parse_tz tz) *. 60.0)));
      (fun string ->
         Scanf.sscanf string "%3s, %d %s %2d %d:%d:%d %s"
           (fun wday mday mon year hour min sec tz ->
              let mon = List.assoc mon months in
              fst (Unix.mktime
                     {Unix.tm_sec=sec; tm_min=min; tm_hour=hour;
                      tm_mday=mday; tm_mon=mon; tm_year=year;
                      tm_wday=0; tm_yday=0; tm_isdst=false})
              -. (float (parse_tz tz) *. 60.0)));
    ]
  
  (* Tries each of the above date parsers, one at a time, until one
     of them doesn't throw an exception. If they all fail, returns
     a value of 0.0. *)
  let getdate string =
    let result = ref 0.0 in
    let parsers = ref date_parsers in
    while !result = 0.0 && !parsers <> [] do
      let parse = List.hd !parsers in
      parsers := List.tl !parsers;
      try result := parse string with _ -> ()
    done;
    !result
  
  (* Formats a date given in epoch seconds for display. *)
  let fmtdate epoch =
    let tm = Unix.localtime epoch in
    Printf.sprintf "%02d:%02d:%02d %04d/%02d/%02d"
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
  
  (* Formats the difference between two epoch times for display. *)
  let fmtdelta delta =
    let sign    = if delta < 0.0 then '-' else ' ' in
    let delta   = abs_float delta in
    let seconds = mod_float delta 60. in
    let delta   = (delta -. seconds) /. 60. in
    let minutes = mod_float delta 60. in
    let delta   = (delta -. minutes) /. 60. in
    let hours   = mod_float delta 24. in
    Printf.sprintf "%c%02.f:%02.f:%02.f" sign hours minutes seconds
  
  (* Process the header for a single email. *)
  let process_header header =
    let start_from =
      try List.assoc "From" header.params
      with Not_found -> header.from in
    let start_from =
      Str.replace_first
        (Str.regexp ".*@\\([^ >]*\\).*") "\\1" start_from in
    let start_date =
      try List.assoc "Date" header.params
      with Not_found -> "" in
    let start_date =
      Str.replace_first
        (Str.regexp " +(.*$") "" start_date in
    let then' = ref (getdate start_date) in
    print_result "Sender" "Recipient" "Time" " Delta";
    print_result "Start" start_from (fmtdate !then') "";
    let prevfrom = ref start_from in
    List.iter
      (fun (key, value) ->
         if key = "Received"
         then
           begin
             let when' =
               Str.replace_first
                 (Str.regexp ".*; +\\(.*\\)$") "\\1" value in
             let when' =
               Str.replace_first
                 (Str.regexp " +(.*$") "" when' in
             let from' =
               try
                 ignore (Str.search_forward
                           (Str.regexp "from +\\([^ )]+\\)") value 0);
                 Str.matched_group 1 value
               with Not_found ->
                 try
                   ignore (Str.search_forward
                             (Str.regexp "(\\([^)]*\\))") value 0);
                   Str.matched_group 1 value
                 with Not_found -> "" in
             let from' = Str.replace_first (Str.regexp ")$") "" from' in
             let by' =
               try
                 ignore (Str.search_forward
                           (Str.regexp "by +\\([^ ]+\\.[^ ]+\\)") value 0);
                 Str.matched_group 1 value
               with Not_found -> "" in
             let now = getdate when' in
             let delta = now -. !then' in
             print_result
               (if !prevfrom <> "" then !prevfrom else from')
               by'
               (fmtdate now)
               (fmtdelta delta);
             then' := now;
             prevfrom := by';
           end)
      (List.rev header.params);
    print_newline ();
    flush stdout
  
  (* Process all emails from standard input. *)
  let () =
    Stream.iter process_header (header_stream_of_channel stdin)
  
  

