(* ********************************************************************** *)
(* Extracting URLs *)
(* ********************************************************************** *)
let pleac_Extracting_URLs () = 
  (* The Nethtml library, part of Ocamlnet, can parse arbitrary HTML from
     files and web pages. *)
  
  #use "topfind";;
  #require "netstring";;
  
  open Nethtml
  
  (* Define a function to walk through all the elements in a document and
     accumulate the results of a user-supplied function for each element.
     This is known as a "fold" in functional programming. *)
  let rec fold_elements f accu = function
    | Element (_, _, children) as element ->
        let accu =
          List.fold_right
            (fun child accu ->
               fold_elements f accu child)
            children
            accu in
        f accu element
    | other -> accu
  
  (* Define a type for links so we can tell anchors and images apart. *)
  type link = A of string | IMG of string
  
  (* Using fold_elements, define a function that collects the URLs from
     all the "a" and "img" tags. *)
  let find_links elements =
    List.flatten
      (List.map
         (fold_elements
            (fun accu element ->
               match element with
                 | Element ("a", attribs, _) ->
                     (try A (List.assoc "href" attribs) :: accu
                      with Not_found -> accu)
                 | Element ("img", attribs, _) ->
                     (try IMG (List.assoc "src" attribs) :: accu
                      with Not_found -> accu)
                 | _ -> accu)
            [])
         elements)
  
  (* Parse an HTML file. *)
  let elements = parse (new Netchannels.input_channel (open_in filename))
  
  (* Print the links we found. *)
  let () =
    List.iter
      (function
         | A href -> Printf.printf "ANCHOR: %s\n" href
         | IMG src -> Printf.printf "IMAGE: %s\n" src)
      (find_links elements)
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* xurl - extract unique, sorted list of links from URL *)
  
  #use "topfind";;
  #require "netclient";;
  
  open Http_client.Convenience
  open Nethtml
  
  let rec fold_elements f accu = function
    | Element (_, _, children) as element ->
        let accu =
          List.fold_right
            (fun child accu ->
               fold_elements f accu child)
            children
            accu in
        f accu element
    | other -> accu
  
  type link = A of string | IMG of string
  
  let find_links elements =
    List.flatten
      (List.map
         (fold_elements
            (fun accu element ->
               match element with
                 | Element ("a", attribs, _) ->
                     (try A (List.assoc "href" attribs) :: accu
                      with Not_found -> accu)
                 | Element ("img", attribs, _) ->
                     (try IMG (List.assoc "src" attribs) :: accu
                      with Not_found -> accu)
                 | _ -> accu)
            [])
         elements)
  
  let base_url = Sys.argv.(1)
  let elements = parse (new Netchannels.input_string (http_get base_url))
  let url_syntax = Hashtbl.find Neturl.common_url_syntax "http"
  let url_syntax =
    {url_syntax with
       Neturl.url_enable_fragment = Neturl.Url_part_allowed}
  let url_syntax = Neturl.partial_url_syntax url_syntax
  
  module StringSet = Set.Make(String)
  
  let () =
    StringSet.iter print_endline
      (List.fold_left
         (fun accu s ->
            try
              StringSet.add
                (Neturl.string_of_url
                   (Neturl.apply_relative_url
                      (Neturl.url_of_string url_syntax base_url)
                      (Neturl.url_of_string url_syntax s)))
                accu
            with Neturl.Malformed_URL ->
              Printf.eprintf "Malformed URL: %s\n%!" s;
              accu)
         StringSet.empty
         (List.map
            (function
               | A href -> href
               | IMG src -> src)
            (find_links elements)))
  

