(* ********************************************************************** *)
(* Finding Fresh Links *)
(* ********************************************************************** *)
let pleac_Finding_Fresh_Links () = 
  #!/usr/bin/ocaml
  (* surl - sort URLs by their last modification date *)
  
  #use "topfind";;
  #require "netclient";;
  
  open Http_client.Convenience
  
  let dates = ref []
  
  let () =
    try
      while true do
        let url = input_line stdin in
        let call = http_head_message url in
        let date =
          try Some (Netdate.parse
                      (call#response_header#field "Last-Modified"))
          with Not_found -> None in
        dates := (date, url) :: !dates
      done
    with End_of_file -> ()
  
  let () =
    List.iter
      (fun (date, url) ->
         Printf.printf "%-25s %s\n"
           (match date with
              | Some date -> Netdate.format "%a %b %d %H:%M:%S %Y" date
              | None -> "<NONE SPECIFIED>")
           url)
      (List.rev (List.sort compare !dates))
  

