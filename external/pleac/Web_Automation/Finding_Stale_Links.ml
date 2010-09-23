(* ********************************************************************** *)
(* Finding Stale Links *)
(* ********************************************************************** *)
let pleac_Finding_Stale_Links () = 
  #!/usr/bin/ocaml
  (* churl - check urls *)
  
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
  
  let check_url url =
    Printf.printf "  %s: %s\n%!" url
      (match (http_head_message url)#status with
         | `Successful -> "OK"
         | _ -> "BAD")
  
  let () =
    if Array.length Sys.argv <> 2
    then (Printf.eprintf "usage: %s <start_url>\n" Sys.argv.(0); exit 1)
  
  let base_url = Sys.argv.(1)
  let elements = parse (new Netchannels.input_string (http_get base_url))
  let url_syntax = Hashtbl.find Neturl.common_url_syntax "http"
  let url_syntax =
    {url_syntax with
       Neturl.url_enable_fragment = Neturl.Url_part_allowed}
  let url_syntax = Neturl.partial_url_syntax url_syntax
  
  module StringSet = Set.Make(String)
  
  let () =
    print_endline (base_url ^ ":");
    StringSet.iter check_url
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
  

