(* ********************************************************************** *)
(* Fetching a URL from a Perl Script *)
(* ********************************************************************** *)
let pleac_Fetching_a_URL_from_a_Perl_Script () = 
  (* If you just want to read a URL as a string, Ocamlnet's "Convenience"
     interface to Http_client is as easy as it gets. For the more powerful
     general interface to Http_client, see the next example. *)
  #use "topfind";;
  #require "netclient";;
  open Http_client.Convenience
  let content = http_get url
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* titlebytes - find the title and size of documents  *)
  
  #use "topfind";;
  #require "str";;
  #require "netclient";;
  
  let raw_url = Sys.argv.(1)
  let url = Neturl.parse_url raw_url
  let () =
    Printf.printf "%s =>\n\t%!" (Neturl.string_of_url url);
    let call = new Http_client.get (Neturl.string_of_url url) in
    call#set_req_header "User-Agent" "Schmozilla/v9.14 Platinum";
    call#set_req_header "Referer" "http://wizard.yellowbrick.oz";
    let pipeline = new Http_client.pipeline in
    pipeline#add call;
    pipeline#run ();
    match call#status with
      | `Successful ->
          let content = call#get_resp_body () in
          let bytes = String.length content in
          let count = ref 0 in
          String.iter (function '\n' -> incr count | _ -> ()) content;
          let regexp =
            Str.regexp_case_fold ".*<title>\\([^<]*\\)</title>.*" in
          let title =
            try (ignore (Str.search_forward regexp content 0);
                 Str.matched_group 1 content)
            with Not_found -> "(untitled)" in
          let title =
            Str.global_replace
              (Str.regexp "\\(^[\n\r\t ]+\\)\\|\\([\n\r\t ]+$\\)") ""
              title in
          Printf.printf "%s (%d lines, %d bytes)\n" title !count bytes
      | `Client_error ->
          Printf.eprintf "Client error: %d %s\n"
            call#response_status_code
            call#response_status_text
      | `Http_protocol_error e ->
          Printf.eprintf "HTTP protocol error: %s\n"
            (Printexc.to_string e)
      | `Redirection ->
          Printf.eprintf "Redirection\n"
      | `Server_error ->
          Printf.eprintf "Server error\n"
      | `Unserved ->
          assert false
  

