(* ********************************************************************** *)
(* Extracting or Removing HTML Tags *)
(* ********************************************************************** *)
let pleac_Extracting_or_Removing_HTML_Tags () = 
  (* Nethtml can be used to safely isolate and extract the text elements
     from an HTML document. *)
  #use "topfind";;
  #require "netstring";;
  
  (* Load the HTML document. *)
  let channel = new Netchannels.input_channel (open_in filename)
  let html = Nethtml.parse channel
  let () = channel#close_in ()
  
  (* Convert the document to plain text. *)
  let plain_text =
    let text = ref [] in
    let rec loop html =
      List.iter
        (function
           | Nethtml.Data s -> text := s :: !text
           | Nethtml.Element (_, _, children) -> loop children)
        html in
    loop (Nethtml.decode html);
    String.concat "" (List.rev !text)
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* htitle - get html title from URL *)
  
  #use "topfind";;
  #require "str";;
  #require "netclient";;
  
  open Http_client.Convenience
  
  let ltrim = Str.global_replace (Str.regexp "^[ \r\n\t\x00\x0B]*") ""
  let rtrim = Str.global_replace (Str.regexp "[ \r\n\t\x00\x0B]*$") ""
  let trim s = rtrim (ltrim s)
  
  let find_title html =
    let title = ref "" in
    let rec loop = function
      | Nethtml.Element ("title", _, Nethtml.Data data :: _) ->
          title := trim data; raise Exit
      | Nethtml.Element (_, _, children) -> List.iter loop children
      | _ -> () in
    (try List.iter loop html with Exit -> ());
    !title
  
  let urls =
    if Array.length Sys.argv > 1
    then List.tl (Array.to_list Sys.argv)
    else (Printf.eprintf "usage: %s url ...\n" Sys.argv.(0); exit 1)
  
  let () =
    List.iter
      (fun url ->
         print_string (url ^ ": ");
         try
           let res = http_get url in
           let ch = new Netchannels.input_string res in
           let html = Nethtml.parse ch in
           print_endline (find_title html)
         with
           | Http_client.Http_error (status, _) ->
               Printf.printf "%d %s\n" status
                 (Nethttp.string_of_http_status
                    (Nethttp.http_status_of_int status))
           | Failure s -> print_endline s)
      urls
  

