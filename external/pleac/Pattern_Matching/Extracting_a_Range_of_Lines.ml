(* ********************************************************************** *)
(* Extracting a Range of Lines *)
(* ********************************************************************** *)
let pleac_Extracting_a_Range_of_Lines () = 
  #load "str.cma";;
  
  (* Creates a stream that produces ranges of items from another stream.
     Production of items starts when when (start_test count item) returns
     true and stops when (finish_test count item) returns true. Multiple
     ranges will be produced if start_test returns true again. The count
     starts at 1. Ranges are inclusive; the item that causes finish_test
     to return true will be produced. *)
  let stream_range start_test finish_test stream =
    let active = ref false in
    let count = ref 1 in
    let rec next i =
      match Stream.peek stream with
        | None -> None
        | Some item ->
            if not !active then
              begin
                if start_test !count item
                then (active := true; next i)
                else (Stream.junk stream; incr count; next i)
              end
            else
              begin
                if finish_test !count item then active := false;
                Stream.junk stream;
                incr count;
                Some item
              end in
    Stream.from next
  
  (* Creates a stream that produces items between a pair of indices.
     If start = 2 and finish = 4, items 2, 3, and 4 will be produced.
     The first item is number 1. *)
  let stream_range_numbers start finish stream =
    stream_range
      (fun count _ -> count = start)
      (fun count _ -> count = finish)
      stream
  
  (* Creates a stream that produces strings between a pair of regexps.
     The regexp will be tested using Str.string_match. *)
  let stream_range_patterns start finish stream =
    stream_range
      (fun _ line -> Str.string_match start line 0)
      (fun _ line -> Str.string_match finish line 0)
      stream
  
  (* Produce a stream of lines from an input channel. *)
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  (* Print lines 15 through 17 inclusive. *)
  let () =
    Stream.iter
      print_endline
      (stream_range_numbers 15 17
         (line_stream_of_channel (open_in datafile)))
  
  (* Print out all <XMP> .. </XMP> displays from HTML doc. *)
  let () =
    Stream.iter
      print_endline
      (stream_range_patterns
         (Str.regexp ".*<XMP>")
         (Str.regexp ".*</XMP>")
         (line_stream_of_channel stdin))
  
  (*-----------------------------*)
  
  let in_header = ref true
  let in_body = ref false
  let () =
    Stream.iter
      (fun line ->
         if !in_header && line = ""
         then (in_header := false; in_body := true)
         else
           begin
             (* do something with line *)
           end)
      (line_stream_of_channel stdin)
  
  (*-----------------------------*)
  
  module StringSet = Set.Make(String)
  let seen = ref StringSet.empty
  let email_regexp = Str.regexp "\\([^<>(),; \t]+@[^<>(),; \t]+\\)"
  let () =
    Stream.iter
      (fun line ->
         List.iter
           (function
              | Str.Delim email ->
                  if not (StringSet.mem email !seen)
                  then
                    begin
                      seen := StringSet.add email !seen;
                      print_endline email;
                    end
              | _ -> ())
           (Str.full_split email_regexp line))
      (stream_range_patterns
         (Str.regexp "^From:?[ \t]")
         (Str.regexp "^$")
         (line_stream_of_channel stdin))
  

