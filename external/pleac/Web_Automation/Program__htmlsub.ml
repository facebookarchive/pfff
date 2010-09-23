(* ********************************************************************** *)
(* Program: htmlsub *)
(* ********************************************************************** *)
let pleac_Program__htmlsub () = 
  #!/usr/bin/ocaml
  (* htmlsub - make substitutions in normal text of HTML files *)
  
  #use "topfind";;
  #require "str";;
  #require "netstring";;
  
  let usage () =
    Printf.eprintf "Usage: %s <from> <to> <file>...\n" Sys.argv.(0);
    exit 1
  
  let from, to', files =
    match List.tl (Array.to_list Sys.argv) with
      | from :: to' :: files -> from, to', files
      | _ -> usage ()
  
  let rec map_data f = function
    | Nethtml.Data data -> Nethtml.Data (f data)
    | Nethtml.Element (name, attribs, children) ->
        Nethtml.Element (name, attribs, List.map (map_data f) children)
  
  let regexp = Str.regexp_string from
  let buffer = Buffer.create 0
  let out_channel = new Netchannels.output_buffer buffer
  let write html = Nethtml.write out_channel (Nethtml.encode html)
  
  let () =
    List.iter
      (fun file ->
         let in_channel = new Netchannels.input_channel (open_in file) in
         let html = Nethtml.decode (Nethtml.parse in_channel) in
         in_channel#close_in ();
         write (List.map (map_data (Str.global_replace regexp to')) html))
      files;
    print_endline (Buffer.contents buffer)
  

