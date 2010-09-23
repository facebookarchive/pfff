(* ********************************************************************** *)
(* Converting ASCII to HTML *)
(* ********************************************************************** *)
let pleac_Converting_ASCII_to_HTML () = 
  #!/usr/bin/ocaml
  (* text2html - trivial html encoding of normal text *)
  
  #use "topfind";;
  #require "str";;
  #require "netstring";;
  
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  let paragraph_stream_of_channel channel =
    let lines = line_stream_of_channel channel in
    let rec next para_lines i =
      match Stream.peek lines, para_lines with
        | None, [] -> None
        | Some "", [] -> Stream.junk lines; next para_lines i
        | Some "", _
        | None, _ -> Some (String.concat "\n" (List.rev para_lines))
        | Some line, _ -> Stream.junk lines; next (line :: para_lines) i in
    Stream.from (next [])
  
  let chop s =
    if s = "" then s else String.sub s 0 (String.length s - 1);;
  
  let substitutions =
    [
      (* embedded URL (good) or guessed URL (bad) *)
      Str.regexp "\\(<URL:[^>]+>\\)\\|\\(http:[^ \n\r\t]+\\)",
      (fun s ->
         let s =
           if s.[0] = '<'
           then String.sub s 5 (String.length s - 6)
           else s in
         [Nethtml.Element ("a", ["href", s], [Nethtml.Data s])]);
  
      (* this is *bold* here *)
      Str.regexp "\\*[^*]+\\*",
      (fun s -> [Nethtml.Element ("strong", [], [Nethtml.Data s])]);
  
      (* this is _italics_ here *)
      Str.regexp "_[^ _]+_",
      (fun s -> [Nethtml.Element ("em", [], [Nethtml.Data s])]);
    ]
  
  let substitute regexp func data =
    List.flatten
      (List.map
         (function
            | Str.Text s -> [Nethtml.Data s]
            | Str.Delim s -> func s)
         (Str.full_split regexp data))
  
  let rec map_data f list =
    List.flatten
      (List.map
         (function
            | Nethtml.Data data -> f data
            | Nethtml.Element (name, attrs, children) ->
                [Nethtml.Element (name, attrs, map_data f children)])
         list)
  
  let text2html text =
    (* Create the initial HTML tree. *)
    let html = [Nethtml.Data text] in
  
    (* Split text into lines. *)
    let html =
      List.flatten
        (List.map
           (function
              | Nethtml.Data data ->
                  List.map
                    (fun line -> Nethtml.Data (line ^ "\n"))
                    ("" :: Str.split (Str.regexp "\n") data)
              | Nethtml.Element _ as e -> [e])
           html) in
  
    (* Perform inline substitutions. *)
    let html =
      List.fold_right
        (fun (regexp, func) ->
           map_data (substitute regexp func))
        substitutions
        html in
  
    (* Add line breaks to quoted text. *)
    let html =
      List.flatten
        (List.map
           (function
              | Nethtml.Data line when line.[0] = '>' ->
                  [Nethtml.Data (chop line);
                   Nethtml.Element ("br", [], []);
                   Nethtml.Data "\n"]
              | Nethtml.Data line -> [Nethtml.Data line]
              | Nethtml.Element _ as e -> [e])
           html) in
  
    (* Return the finished document. *)
    html
  
  let buffer = Buffer.create 0
  let channel = new Netchannels.output_buffer buffer
  let write html = Nethtml.write channel (Nethtml.encode html)
  let paragraphs = paragraph_stream_of_channel stdin
  
  (* Main loop *)
  let () =
    let first = ref true in
    Stream.iter
      (fun para ->
         if !first then first := false
         else write [Nethtml.Data "\n\n"];
         (* Paragraphs beginning with whitespace are wrapped in <pre> *)
         let tag, body =
           if String.length para > 0 && String.contains " \t" para.[0]
           then "pre", [Nethtml.Data "\n";
                        Nethtml.Data para;  (* indented verbatim *)
                        Nethtml.Data "\n"]
           else "p", text2html para in      (* add paragraph tag *)
         write [Nethtml.Element (tag, [], body)])
      paragraphs;
    print_endline (Buffer.contents buffer)
  
  (*-----------------------------*)
  
  (* To format mail headers as a table, add the following just before
     the main loop. *)
  let () =
    let colon_delim = Str.regexp "[ \t]*:[ \t]*" in
    let continuation = Str.regexp "\n[ \t]+" in
    try
      let headers = Stream.next paragraphs in
      let headers = Str.global_replace continuation " " headers in
      let lines = Str.split (Str.regexp "\n") headers in
      let rows =
        List.flatten
          (List.map
             (fun line ->
                (* parse heading *)
                let key, value =
                  match Str.bounded_split_delim colon_delim line 2 with
                    | [key; value] -> key, value
                    | _ -> "", line in
                [Nethtml.Element
                   ("tr", [],
                    [Nethtml.Element
                       ("th", ["align", "left"], [Nethtml.Data key]);
                     Nethtml.Element
                       ("td", [], [Nethtml.Data value])]);
                 Nethtml.Data "\n"])
             lines) in
      write [Nethtml.Element ("table", [], Nethtml.encode rows);
             Nethtml.Element ("hr", [], []);
             Nethtml.Data "\n\n"]
    with Stream.Failure -> ()
  

